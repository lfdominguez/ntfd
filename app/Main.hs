module Main
    ( main
    )
where

import Control.Concurrent.Async (async, waitAny)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Bifunctor (first)
import Data.Either (rights)
import Data.Time.Clock (NominalDiffTime)
import DBus.Client
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.Exit (exitFailure)

import Config (loadConfig, Config(..), ConfigError(..), WeatherConfig(..))
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
    requestResult <- requestName client "io.ntfd.services" []
    when (requestResult /= NamePrimaryOwner) $ do
        putStrLn "Another service is connected to DBus as \"io.ntfd.services\""
        exitFailure

    -- Prepare services
    let weatherSvc = first WeatherConfigErr $ weatherService client <$> weatherCfg config

    -- Spawn threads and wait forever, or until one of them fail
    services <- mapM async $ rights [weatherSvc]
    _        <- waitAny services
    exitFailure

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
        { interfaceName       = "io.ntfd.openweathermap"
        , interfaceProperties = properties s
        }
    renderedTemplate s = readOnlyProperty "RenderedTemplate" $ orEmpty $ WS.getRenderedTemplate s
    properties s = [renderedTemplate s]

-- D-Bus has no null or empty type, this maps errors to mempty
orEmpty :: Monoid t => IO (Either e t) -> IO t
orEmpty query = do
    res <- query
    pure $ case res of
        Left  _ -> mempty
        Right t -> t

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . toMicroSeconds where toMicroSeconds = (1000000 *) . fromInteger . round
