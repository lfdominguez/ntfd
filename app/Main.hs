module Main
    ( main
    )
where

import Control.Monad (unless, when)
import Control.Concurrent.Async (async, waitAny)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import DBus.Client (connectSession, requestName, RequestNameReply(..))
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.Exit (exitFailure)

import Config (loadConfig, Config(..))
import Modules.Github (githubStringsSvc)
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

    -- Prepare services, partition by config validity
    let allServices =
            [ githubStringsSvc client <$> first ("Github", ) (githubCfg config)
            , weatherStringsSvc client <$> first ("Weather", ) (weatherCfg config)
            , mpdNotifSvc client <$> first ("mpd", ) (mpdCfg config)
            ]
        (invalid, valid) = partitionEithers allServices

    -- Log which services failed to initialize / are disabled
    unless (null invalid) $ do
        putStrLn "The following modules were ignored: "
        mapM_ (\(name, err) -> putStrLn $ " - " <> name <> " (" <> show err <> ")") invalid
        putStrLn ""

    -- Make sure at least one module is enabled
    when (null valid) $ do
        putStrLn "Could not start any module, exiting."
        exitFailure

    -- Spawn threads and wait forever, or until one of them fails
    services <- mapM async valid
    _        <- waitAny services
    exitFailure
