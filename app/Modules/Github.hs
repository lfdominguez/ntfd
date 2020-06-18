module Modules.Github
    ( githubStringsSvc
    )
where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forever)
import Data.Either (fromRight)
import Data.Int (Int32)
import Data.Text (pack)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Word (Word32)
import DBus.Client
    (defaultInterface, export, readOnlyProperty, interfaceName, interfaceProperties, Client)
import System.Random (randomRIO)

import Config (GithubConfig(..))
import Helpers (notify, sleep, NotificationType(..))
import qualified Stores.Github as GH

-- "all strings" version of the github DBus API, this is meant to be convenient to use
-- from shell scripts, errors are mapped to empty strings (since DBus has no null/empty type)
githubStringsSvc :: Client -> GithubConfig -> IO ()
githubStringsSvc dbusClient config = do
    putStrLn "Started Github synchronization service."
    (store :: GH.GithubClient) <- GH.initGithubStore config
    export dbusClient "/github" $ interface store
    forever $ do
        syncRes <- GH.syncGithub store
        case syncRes of
            Right notifyIds -> do
                ns <- filterNew store notifyIds
                sendNotifications ns
                if not $ null notifyIds
                    then sleep $ githubSyncFreq config - delay
                    else sleep $ githubSyncFreq config

            _ -> sleep $ githubSyncFreq config
  where
    delay = secondsToNominalDiffTime 15 -- Keep notifications in sync with template
    interface s =
        defaultInterface { interfaceName = "github.strings", interfaceProperties = properties s }
    properties s =
        [ readOnlyProperty "RenderedTemplate" $ fromRight "" <$> GH.getRenderedTemplate s
        , readOnlyProperty "FirstTimeNotifications" $ toInt (GH.getFistTimeNotificationCount s)
        , readOnlyProperty "UnreadNotifications" $ toInt (GH.getUnreadNotificationCount s)
        ]
    toInt value = do
        maybeNatural <- value
        pure $ maybe 0 fromIntegral maybeNatural :: IO Int32
    filterNew store newIds = do
        ns <- GH.getNotifications store
        pure $ filter (isNewNotification newIds) ns
    isNewNotification ns n =
        let notifId = (GH.notificationId . GH.restNotification) n in elem notifId ns
    sendNotifications = mapConcurrently_ sendNotification
    sendNotification n = do
        rid <- randomRIO (50, maxBound :: Word32)
        let timeout     = githubNotifTime config
        let nType = pack . show $ (GH.notificationType . GH.restNotification) n
        let description = (GH.title . GH.restNotification) n
        let title       = (GH.repoFullName . GH.restNotification) n
        let
            body = if (GH.isNew . GH.restNotification) n
                then "New GitHub " <> nType <> "\n\n" <> description
                else "Activity in GitHub " <> nType <> "\n\n" <> description
        let icon = if githubShowAvatar config then Just $ GH.avatarPath n else Nothing
        sleep delay
        notify dbusClient (Github rid) title body icon timeout
