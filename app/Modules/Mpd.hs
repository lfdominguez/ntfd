module Modules.Mpd
    ( mpdNotifSvc
    )
where

import Control.Monad (forever, when)
import DBus.Client (Client)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.MPD
    (currentSong, idle, toString, toText, withMPD, Metadata(..), Song(..), Subsystem(..))
import System.Directory (doesFileExist)
import System.FilePath (joinPath, splitFileName)
import qualified Data.Map as M
import qualified Data.Text as T

import Helpers (notify, NotificationType(..))
import Config (MpdConfig(..))

-- MPD notification service, watches player state changes and sends
-- notifications on track change
mpdNotifSvc :: Client -> MpdConfig -> IO ()
mpdNotifSvc client config = do
    putStrLn "Started MPD notification service."
    songRef <- newIORef ""
    forever $ do
        -- Block until the player state changes
        _       <- withMPD $ idle [PlayerS]
        -- Query the current song name and compare it
        -- to what we have in the IO ref
        newSong <- withMPD currentSong
        case newSong of
            Right (Just s) -> processSong songRef s
            _              -> pure ()
  where
    processSong ref song = do
        let newSongPath = (toString . sgFilePath) song
        currentPath <- readIORef ref
        when (newSongPath /= currentPath) $ do
            writeIORef ref newSongPath
            sendNotification song
    sendNotification song = do
        let tags    = sgTags song
        let sTitle  = M.lookup Title tags
        let sArtist = M.lookup Artist tags
        let sAlbum  = M.lookup Album tags
        cover <- getCoverPath song
        case (sTitle, sArtist, sAlbum) of
            (Just [title], Just [artist], Just [album]) ->
                let
                    nHead = toText title
                    nBody = toText artist <> " - " <> toText album
                in notify client Mpd nHead nBody cover
            _ -> pure ()
    getCoverPath song = do
        let musicDir     = mpdMusicDirectory config
        let coverFile    = mpdCoverName config
        let (songDir, _) = splitFileName $ (toString . sgFilePath) song
        let coverPath    = joinPath [musicDir, songDir, coverFile]
        hasCover <- doesFileExist coverPath
        pure $ if hasCover then Just (T.pack coverPath) else Nothing
