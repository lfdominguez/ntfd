module Config.Mpd
    ( loadMpdConfig
    , MpdConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import System.Directory (doesDirectoryExist)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec)
import qualified Toml

import Config.Env (expandPath)
import Config.Error (ConfigError(..))
import Helpers (toDiffTime)

-- | Load mpd configuration from raw TOML content
loadMpdConfig :: Text -> IO (Either ConfigError MpdConfig)
loadMpdConfig toml = do
    let decoded = decode (Toml.table mpdCodec "mpd") toml
    case first ParseError decoded of
        Left  e      -> pure $ Left e
        Right parsed -> withEnv parsed
  where
    withEnv parsed = do
        musicDir       <- expandPath $ musicDirectory parsed
        musicDirExists <- doesDirectoryExist musicDir
        pure $ build musicDir musicDirExists parsed
    build musicDir musicDirExists parsed
        | not (enabled parsed) = Left Disabled
        | not musicDirExists = Left InvalidPath
        | otherwise = Right $ MpdConfig
            { mpdEnabled          = enabled parsed
            , mpdMusicDirectory   = musicDir
            , mpdCoverName        = coverName parsed
            , mpdNotifTimeout     = toDiffTime $ notifTimeout parsed
            , mpdSkipMissingCover = skipMissingCover parsed
            }

-- | MPD configuration options required by the application
data MpdConfig = MpdConfig
    { mpdEnabled :: Bool
    , mpdMusicDirectory :: FilePath
    , mpdCoverName :: String
    , mpdNotifTimeout :: NominalDiffTime
    , mpdSkipMissingCover :: Bool
    } deriving (Show)

-- | MPD configuration options as they can be read from the TOML configuration file
data TomlMpdConfig = TomlMpdConfig
    { enabled :: Bool
    , musicDirectory :: FilePath
    , coverName :: String
    , notifTimeout :: Natural
    , skipMissingCover :: Bool
    }

-- brittany-disable-next-binding
mpdCodec :: TomlCodec TomlMpdConfig
mpdCodec = TomlMpdConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.string "music_directory" .= musicDirectory
    <*> Toml.string "cover_name" .= coverName
    <*> Toml.natural "notification_timeout" .= notifTimeout
    <*> Toml.bool "skip_missing_cover" .= skipMissingCover
