module Main
    ( main
    )
where

import Control.Concurrent.Async (async, waitAny)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import DBus.Client
    ( autoMethod
    , connectSession
    , defaultInterface
    , requestName
    , export
    , readOnlyProperty
    , interfaceName
    , interfaceMethods
    , interfaceProperties
    , Client
    , RequestNameReply(..)
    )
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.Exit (exitFailure)

import Config (loadConfig, Config(..), ConfigError(..), WeatherConfig(..))
import Types.Weather (convert, Temperature(..), Unit(..))
import qualified Stores.Weather as WS

main :: IO ()
main = do
    -- Read config file
    path   <- getXdgDirectory XdgConfig "ntfd"
    cfgRes <- loadConfig $ path <> "/config.toml"
    config <- case cfgRes of
        Left e -> do
            putStrLn $ "Failed to read config file: " <> show e
            exitFailure
        Right c -> pure c

    -- Connect to DBus
    client        <- connectSession
    requestResult <- requestName client "io.ntfd" []
    when (requestResult /= NamePrimaryOwner) $ do
        putStrLn "Another service is connected to DBus as \"io.ntfd\""
        exitFailure

    -- Prepare services
    let weatherSvc = first WeatherConfigErr $ weatherService client <$> weatherCfg config

    -- Spawn threads and wait forever, or until one of them fails
    services <- mapM async $ rights [weatherSvc]
    _        <- waitAny services
    exitFailure

-- "all strings" version of the weather DBus API, this is meant to be convenient to use
-- from shell scripts, errors are mapped to empty strings (since DBus has no null/empty type)
weatherService :: Client -> WeatherConfig -> IO ()
weatherService dbusClient config = do
    putStrLn "Started OpenWeatherMap synchronization service."
    (store :: WS.WeatherClient) <- WS.initWeatherStore config
    export dbusClient "/weather" $ interface store
    forever $ do
        _ <- WS.syncForecast store
        sleep $ weatherSyncFreq config
  where
    interface s = defaultInterface
        { interfaceName       = "openweathermap.strings"
        , interfaceMethods    = methods s
        , interfaceProperties = properties s
        }
    renderedTemplate s =
        readOnlyProperty "RenderedTemplate" $ fromEither <$> WS.getRenderedTemplate s
    currentIcon s = readOnlyProperty "CurrentIcon" $ fromIcon <$> WS.getCurrentIcon s
    forecastIcon s = readOnlyProperty "ForecastIcon" $ fromIcon <$> WS.getForecastIcon s
    currentTemp s = autoMethod "CurrentTemperature" $ currentTemperature s
    forecastTemp s = autoMethod "ForecastTemperature" $ forecastTemperature s
    methods s = [currentTemp s, forecastTemp s]
    properties s = [renderedTemplate s, currentIcon s, forecastIcon s]

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds where toMicroSeconds = (1000000 *) . fromInteger . round

-- The following functions help mapping our data types to work around DBus' lack of null type
fromEither :: (Monoid t) => Either e t -> t
fromEither (Left  _) = mempty
fromEither (Right t) = t

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
    | u == "celcius"    = asValue Celcius
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
