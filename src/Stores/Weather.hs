module Stores.Weather
    ( Store(..)
    , Error(..)
    , WeatherClient
    )
where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (newEmptyMVar, MVar)
import Data.Text (Text)

import Clients.OpenWeatherMap (fetchOwm, Error(..), QueryType(..))
import Types.Weather (Temperature)
import Config (WeatherConfig(..))

-- | Query and update Weather data.
class Store s where
    -- | Initialize a Weather store.
    init :: WeatherConfig -> IO s
    -- | Synchronize weather data.
    syncForecast :: s -> IO (Either Error ())
    -- | Get rendered template.
    displayForecast :: s -> IO (Maybe Text)

-- Main client, implements the Store typeclass
data WeatherClient = WeatherClient
    { config :: WeatherConfig
    , weatherData :: MVar WeatherData
    }

-- Holds weather state
data WeatherData = WeatherData
    { currentTemperature :: Temperature
    , currentIcon :: Char
    , forecastTemperature :: Temperature
    , forecastIcon :: Char
    , renderedTemplate :: Text
    }

instance Store WeatherClient where
    init config = do
        weatherData <- newEmptyMVar
        pure WeatherClient { config, weatherData }

    syncForecast s = undefined
        -- do
        -- let cfg = config s
        -- [current, forecast] <- mapConcurrently (fetchOwm cfg) [Current, Forecast]
        -- pure $ Left Parse

    displayForecast _ = undefined
