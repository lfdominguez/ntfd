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
import Clients.OpenWeatherMap
    (fetchOwm, isDegradedConditions, toSymbolicName, toWeatherIcon, OwmResponse(..), QueryType(..))
import Types.Weather (convert, Temperature(..), Unit(..))
import Config (WeatherConfig(..))

-- | Query and update Weather data.
class Store s where
    -- | Initialize the store
    initWeatherStore :: WeatherConfig -> IO s
    -- | Synchronize local data with the weather service.
    -- Boolean value can be used to know whether or not notifications should be sent.
    syncForecast :: s -> IO (Either Error Bool)
    -- | Get a rendered version of the configured template
    getRenderedTemplate :: s -> IO (Either Error Text)
    -- | Get current temperature
    getCurrentTemperature :: s -> IO (Maybe Temperature)
    -- | Get forecast temperature
    getForecastTemperature :: s -> IO (Maybe Temperature)
    -- | Get current icon from the WeatherIcon font
    getCurrentIcon :: s -> IO (Maybe Char)
    -- | Get forecast icon from the WeatherIcon font
    getForecastIcon :: s -> IO (Maybe Char)
    -- | Get current description in the configured language
    getCurrentDescription :: s -> IO (Maybe Text)
    -- | Get forecast description in the configured language
    getForecastDescription :: s -> IO (Maybe Text)
    -- | Get current symbolic icon name according to the Icon Theme Specification
    getCurrentSymbolic :: s -> IO (Maybe Text)
    -- | Get forecast symbolic icon name according to the Icon Theme Specification
    getForecastSymbolic :: s -> IO (Maybe Text)

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
    { currentStatus :: Int
    , forecastStatus :: Int
    , currentTemperature :: Temperature
    , forecastTemperature :: Temperature
    , currentIcon :: Char
    , forecastIcon :: Char
    , currentDescription :: Text
    , forecastDescription :: Text
    , currentSymbolic :: Text
    , forecastSymbolic :: Text
    }

instance Store WeatherClient where
    initWeatherStore config = do
        internalState <- newEmptyMVar
        pure WeatherClient { .. }

    syncForecast s = do
        [current, forecast] <- mapConcurrently (fetchOwm cfg) [Current, Forecast]
        case (current, forecast) of
            -- When both calls succeed, we update the store
            (Right c, Right f) -> do
                -- Build new state
                let currentStatus       = owmStatus c
                    forecastStatus      = owmStatus f
                    currentTemperature  = toTemperature c
                    forecastTemperature = toTemperature f
                    currentIcon         = toIcon c
                    forecastIcon        = toIcon f
                    currentDescription  = owmDescription c
                    forecastDescription = owmDescription f
                    currentSymbolic     = toSymbolicName currentStatus
                    forecastSymbolic    = toSymbolicName forecastStatus
                    wData               = WeatherData { .. }
                    renderedTemplate    = renderTemplate wData template
                    state               = InternalState { weatherData = wData, .. }
                -- Update store
                oldState <- tryTakeMVar mvar
                putMVar mvar state
                pure $ Right $ shouldNotify (weatherData <$> oldState) forecastStatus
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
        shouldNotify Nothing newStatus = newStatus < 800
        shouldNotify (Just WeatherData { forecastStatus = old }) new = isDegradedConditions old new

    getRenderedTemplate s = do
        state <- tryReadMVar $ internalState s
        pure $ case state of
            Nothing -> Left Unsynchronized
            Just w  -> renderedTemplate w

    getCurrentTemperature s = do
        state <- tryReadMVar $ internalState s
        pure $ currentTemperature . weatherData <$> state

    getCurrentDescription s = do
        state <- tryReadMVar $ internalState s
        pure $ currentDescription . weatherData <$> state

    getCurrentSymbolic s = do
        state <- tryReadMVar $ internalState s
        pure $ currentSymbolic . weatherData <$> state

    getCurrentIcon s = do
        state <- tryReadMVar $ internalState s
        pure $ currentIcon . weatherData <$> state

    getForecastTemperature s = do
        state <- tryReadMVar $ internalState s
        pure $ forecastTemperature . weatherData <$> state

    getForecastDescription s = do
        state <- tryReadMVar $ internalState s
        pure $ forecastDescription . weatherData <$> state

    getForecastSymbolic s = do
        state <- tryReadMVar $ internalState s
        pure $ forecastSymbolic . weatherData <$> state

    getForecastIcon s = do
        state <- tryReadMVar $ internalState s
        pure $ forecastIcon . weatherData <$> state

-- brittany-disable-next-binding
renderTemplate :: WeatherData -> Text -> Either Error Text
renderTemplate w t = do
    -- Compile user provided template
    template <- first Parse $ compileMustacheText "weather template" $ fromStrict t
    -- Render it
    case renderMustacheW template payload of
        ([]  , res) -> Right $ toStrict res
        (errs, _  ) -> Left $ Render errs
  where
    payload = object
        [ "temp_celcius" .= valueAs Celcius current
        , "temp_kelvin" .= valueAs Kelvin current
        , "temp_fahrenheit" .= valueAs Fahrenheit current
        , "temp_icon" .= currentIcon w
        , "trend" .= trend current forecast
        , "forecast_celcius" .= valueAs Celcius forecast
        , "forecast_kelvin" .= valueAs Kelvin forecast
        , "forecast_fahrenheit" .= valueAs Fahrenheit forecast
        , "forecast_icon" .= forecastIcon w
        ]
    trend c f
        | c < f = '\59621' -- ^ trending up
        | c > f = '\59619' -- ^ trending down
        | otherwise = '\59620' -- ^ flat
    valueAs target temp = let Temperature value _ = convert temp target in round value :: Int
    current = currentTemperature w
    forecast = forecastTemperature w

-- | Error types the store might return.
data Error
    = Parse ParseError
    | Render [MustacheWarning]
    | Unsynchronized
    | Owm [Owm.Error]
    deriving (Show)
