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
import DBus (methodCall, methodCallDestination, methodCallBody, toVariant, Variant)
import DBus.Client
    ( autoMethod
    , callNoReply
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
import Data.Int (Int32)
import Data.Word (Word32)
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.Exit (exitFailure)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Map as M

import Config (loadConfig, Config(..), ConfigError(..), WeatherConfig(..))
import Types.Weather (convert, Temperature(..), Unit(..))
import qualified Stores.Weather as WS

data NotificationType
    = Weather
    | Twitch
    deriving (Show, Eq)

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
        shouldNotify <- WS.syncForecast store
        case shouldNotify of
            Right True -> do
                let body = weatherNotifBody config
                title <- fromMaybe <$> WS.getForecastDescription store
                icon  <- WS.getForecastSymbolic store
                notify dbusClient Weather title body icon
            _ -> pure ()
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

notify :: Client -> NotificationType -> Text -> Text -> Maybe Text -> IO ()
notify client nType title text icon = callNoReply client params
  where
    params = (methodCall objectPath interface methodName)
        { methodCallDestination = Just "org.freedesktop.Notifications"
        , methodCallBody        = args
        }
    objectPath = "/org/freedesktop/Notifications"
    interface  = "org.freedesktop.Notifications"
    methodName = "Notify"
    appName    = "ntfd" :: Text
    appIcon    = fromMaybe icon
    replaceId nType'
        | nType' == Weather = 1
        | nType' == Twitch  = 2
        | otherwise         = 0
    args =
        [ toVariant appName
        , toVariant (replaceId nType :: Word32)
        , toVariant appIcon
        , toVariant $ capitalize title
        , toVariant text
        , toVariant ([] :: [String])
        , toVariant (M.fromList [] :: M.Map String Variant)
        , toVariant (10000 :: Int32)
        ]

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds where toMicroSeconds = (1000000 *) . fromInteger . round

capitalize :: Text -> Text
capitalize text = case T.uncons text of
    Nothing     -> text
    Just (h, t) -> let lowered = T.toLower t in T.cons (C.toUpper h) lowered

-- The following functions help mapping our data types to work around DBus' lack of null type
fromEither :: (Monoid t) => Either e t -> t
fromEither (Left  _) = mempty
fromEither (Right t) = t

fromMaybe :: (Monoid t) => Maybe t -> t
fromMaybe Nothing  = mempty
fromMaybe (Just t) = t

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
