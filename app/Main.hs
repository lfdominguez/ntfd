module Main
    ( main
    )
where

import Control.Monad (when)
import Control.Concurrent.Async (async, waitAny)
import Data.Either (rights)
import DBus.Client (connectSession, requestName, RequestNameReply(..))
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.Exit (exitFailure)

import Config (loadConfig, Config(..))
import Modules.Weather (weatherStringsSvc)
import Modules.Mpd (mpdNotifSvc)

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
    let weatherSvc  = weatherStringsSvc client <$> weatherCfg config
    let mpdSvc      = mpdNotifSvc client <$> mpdCfg config
    let allServices = [weatherSvc, mpdSvc]

    -- Log which services failed to initialize / are disabled
    -- print $ lefts allServices

    -- Spawn threads and wait forever, or until one of them fails
    services <- mapM async $ rights allServices
    _        <- waitAny services
    exitFailure
