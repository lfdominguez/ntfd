module Config.Mpd
    ( loadMpdConfig
    , MpdConfig(..)
    , MpdCfgError
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Toml ((.=), decode, TomlCodec, DecodeException)
import System.FilePath (isValid)
import qualified Toml

import Config.Global (GlobalConfig(..))

-- | Load mpd configuration from raw TOML content
loadMpdConfig :: Text -> GlobalConfig -> Either MpdCfgError MpdConfig
loadMpdConfig toml global = do
    parsed <- first ParseError $ decode (Toml.table mpdCodec "mpd") toml
    fromToml parsed
  where
    fromToml toml'
        | not (enabled toml') = Left Disabled
        | not (isValid (musicDirectory toml')) = Left InvalidPath
        | otherwise = Right $ MpdConfig
            { mpdGlobalConf       = global
            , mpdEnabled          = enabled toml'
            , mpdMusicDirectory   = musicDirectory toml'
            , mpdCoverName        = coverName toml'
            , mpdSkipMissingCover = skipMissingCover toml'
            }

-- | MPD configuration options required by the application
data MpdConfig = MpdConfig
    { mpdGlobalConf :: GlobalConfig
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

data MpdCfgError
    = Disabled
    | InvalidPath
    | ParseError DecodeException
    deriving (Show)

-- brittany-disable-next-binding
mpdCodec :: TomlCodec TomlMpdConfig
mpdCodec = TomlMpdConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.string "music_directory" .= musicDirectory
    <*> Toml.string "cover_name" .= coverName
    <*> Toml.bool "skip_missing_cover" .= skipMissingCover
