module Modules.Weather
    ( weatherStringsSvc
    )
where

import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Text (unpack, Text)
import DBus.Client
    ( autoMethod
    , defaultInterface
    , export
    , readOnlyProperty
    , interfaceName
    , interfaceMethods
    , interfaceProperties
    , Client
    )

import Config (WeatherConfig(..))
import Types.Weather (convert, Temperature(..), Unit(..))
import Helpers (capitalize, notify, sleep, NotificationType(..))
import qualified Stores.Weather as WS

-- "all strings" version of the weather DBus API, this is meant to be convenient to use
-- from shell scripts, errors are mapped to empty strings (since DBus has no null/empty type)
weatherStringsSvc :: Client -> WeatherConfig -> IO ()
weatherStringsSvc dbusClient config = do
    putStrLn "Started OpenWeatherMap synchronization service."
    (store :: WS.WeatherClient) <- WS.initWeatherStore config
    export dbusClient "/weather" $ interface store
    forever $ do
        shouldNotify <- WS.syncForecast store
        case shouldNotify of
            Right True -> do
                let body    = weatherNotifBody config
                let timeout = weatherNotifTime config
                title <- capitalize . fromMaybe "" <$> WS.getForecastDescription store
                icon  <- WS.getForecastSymbolic store
                sleep delay
                notify dbusClient Weather title body (unpack <$> icon) timeout
                sleep $ weatherSyncFreq config - delay

            _ -> sleep $ weatherSyncFreq config
  where
    interface s = defaultInterface
        { interfaceName       = "openweathermap.strings"
        , interfaceMethods    = methods s
        , interfaceProperties = properties s
        }
    renderedTemplate s =
        readOnlyProperty "RenderedTemplate" $ fromRight "" <$> WS.getRenderedTemplate s
    currentIcon s = readOnlyProperty "CurrentIcon" $ fromIcon <$> WS.getCurrentIcon s
    forecastIcon s = readOnlyProperty "ForecastIcon" $ fromIcon <$> WS.getForecastIcon s
    currentTemp s = autoMethod "CurrentTemperature" $ currentTemperature s
    forecastTemp s = autoMethod "ForecastTemperature" $ forecastTemperature s
    methods s = [currentTemp s, forecastTemp s]
    properties s = [renderedTemplate s, currentIcon s, forecastIcon s]
    delay = secondsToNominalDiffTime 65 -- Keep notifications in sync with template

fromIcon :: Maybe Char -> String
fromIcon Nothing  = ""
fromIcon (Just c) = "" ++ [c]

currentTemperature :: WS.Store c => c -> Text -> IO String
currentTemperature client unit = do
    current <- WS.getCurrentTemperature client
    pure $ toTemperatureValue unit current

forecastTemperature :: WS.Store c => c -> Text -> IO String
forecastTemperature client unit = do
    current <- WS.getForecastTemperature client
    pure $ toTemperatureValue unit current

toTemperatureValue :: Text -> Maybe Temperature -> String
toTemperatureValue _ Nothing = ""
toTemperatureValue u (Just t)
    | u == "celsius"    = asValue Celsius
    | u == "kelvin"     = asValue Kelvin
    | u == "fahrenheit" = asValue Fahrenheit
    | otherwise         = ""
  where
    value (Temperature v _) = v
    asValue unit =
        let
            converted = convert t unit
            rounded   = round $ value converted :: Integer
        in show rounded
