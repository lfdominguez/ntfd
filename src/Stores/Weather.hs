module Stores.Weather
    ( Store(..)
    , Error(..)
    , WeatherClient
    )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (newEmptyMVar, tryReadMVar, tryTakeMVar, putMVar, MVar)
import Data.Aeson ((.=), object)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Text.Microstache (compileMustacheText, renderMustacheW, MustacheWarning)
import Text.Parsec (ParseError)

import qualified Clients.OpenWeatherMap as Owm
import Clients.OpenWeatherMap (fetchOwm, toWeatherIcon, OwmResponse(..), QueryType(..))
import Types.Weather (convert, Temperature(..), Unit(..))
import Config (WeatherConfig(..))

-- | Query and update Weather data.
class Store s where
    -- | Initialize a Weather store.
    init :: WeatherConfig -> IO s
    -- | Synchronize weather data.
    syncForecast :: s -> IO (Either Error ())
    -- | Get rendered template.
    displayForecast :: s -> IO (Either Error Text)

-- Main client, implements the Store typeclass
data WeatherClient = WeatherClient
    { config :: WeatherConfig
    , internalState :: MVar InternalState
    }

-- Holds internal state: rendered template and weather data from OWM
data InternalState = InternalState
    { weatherData :: WeatherData
    , renderedTemplate :: Either Error Text
    }

-- Actual weather data
data WeatherData = WeatherData
    { currentTemperature :: Temperature
    , currentIcon :: Char
    , forecastTemperature :: Temperature
    , forecastIcon :: Char
    }

instance Store WeatherClient where
    init config = do
        internalState <- newEmptyMVar
        pure WeatherClient { config, internalState }

    syncForecast s = do
        [current, forecast] <- mapConcurrently (fetchOwm cfg) [Current, Forecast]
        case (current, forecast) of
            -- When both calls succeed, we update the store
            (Right c, Right f) -> do
                -- Build new state
                let currentTemperature  = toTemperature c
                    currentIcon         = toIcon c
                    forecastTemperature = toTemperature f
                    forecastIcon        = toIcon f
                    weatherData         = WeatherData { .. }
                    renderedTemplate    = renderTemplate weatherData template
                    state               = InternalState { .. }
                -- Update store
                _ <- tryTakeMVar mvar
                putMVar mvar state
                pure $ Right ()
            -- Report failure otherwise
            (Left c, Left f) -> pure $ Left $ Owm [c, f]
            (Left c, _     ) -> pure $ Left $ Owm [c]
            (_     , Left f) -> pure $ Left $ Owm [f]
      where
        cfg      = config s
        mvar     = internalState s
        template = weatherTemplate cfg
        toTemperature res = Temperature (owmTemperature res) Celcius
        toIcon res = toWeatherIcon $ owmIcon res

    displayForecast s = do
        state <- tryReadMVar $ internalState s
        pure $ case state of
            Nothing -> Left Unsynchronized
            Just w  -> renderedTemplate w

-- brittany-disable-next-binding
renderTemplate :: WeatherData -> Text -> Either Error Text
renderTemplate w t = do
    template <- first Parse $ compileMustacheText "weather template" $ fromStrict t
    case renderMustacheW template payload of
        ([]  , res) -> Right $ toStrict res
        (errs, _  ) -> Left $ Render errs
  where
    payload = object
        [ "temp_celcius" .= (valueAs current Celcius :: Float)
        , "temp_kelvin" .= (valueAs current Kelvin :: Float)
        , "temp_fahrenheit" .= (valueAs current Fahrenheit :: Float)
        , "temp_icon" .= (currentIcon w :: Char)
        , "trend" .= (trend current forecast :: Char)
        , "forecast_celcius" .= (valueAs forecast Celcius :: Float)
        , "forecast_kelvin" .= (valueAs forecast Kelvin :: Float)
        , "forecast_fahrenheit" .= (valueAs forecast Fahrenheit :: Float)
        , "forecast_icon" .= (forecastIcon w :: Char)
        ]
    trend c f
        | c < f = '\59621' -- ^ trending up
        | c > f = '\59619' -- ^ trending down
        | otherwise = '\59620' -- ^ flat
    valueAs temp target = let Temperature value _ = convert temp target in value
    current = currentTemperature w
    forecast = forecastTemperature w

-- | Error types the store might return.
data Error
    = Parse ParseError
    | Render [MustacheWarning]
    | Unsynchronized
    | Owm [Owm.Error]
    deriving (Show)
