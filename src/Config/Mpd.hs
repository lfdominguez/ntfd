module Config.Mpd
    ( loadMpdConfig
    , MpdConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Toml ((.=), decode, TomlCodec)
import System.Directory (doesDirectoryExist)
import qualified Toml

import Config.Env (expandPath)
import Config.Error (ConfigError(..))
import Config.Global (GlobalConfig(..))

-- | Load mpd configuration from raw TOML content
loadMpdConfig :: Text -> GlobalConfig -> IO (Either ConfigError MpdConfig)
loadMpdConfig toml global = do
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
            { mpdGlobalCfg        = global
            , mpdEnabled          = enabled parsed
            , mpdMusicDirectory   = musicDir
            , mpdCoverName        = coverName parsed
            , mpdSkipMissingCover = skipMissingCover parsed
            }

-- | MPD configuration options required by the application
data MpdConfig = MpdConfig
    { mpdGlobalCfg :: GlobalConfig
    , mpdEnabled :: Bool
    , mpdMusicDirectory :: FilePath
    , mpdCoverName :: String
    , mpdSkipMissingCover :: Bool
    } deriving (Show)

-- | MPD configuration options as they can be read from the TOML configuration file
data TomlMpdConfig = TomlMpdConfig
    { enabled :: Bool
    , musicDirectory :: FilePath
    , coverName :: String
    , skipMissingCover :: Bool
    }

-- brittany-disable-next-binding
mpdCodec :: TomlCodec TomlMpdConfig
mpdCodec = TomlMpdConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.string "music_directory" .= musicDirectory
    <*> Toml.string "cover_name" .= coverName
    <*> Toml.bool "skip_missing_cover" .= skipMissingCover
