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

-- | Load mpd configuration from raw TOML content
loadMpdConfig :: Text -> Either MpdCfgError MpdConfig
loadMpdConfig toml = do
    parsed <- first ParseError $ decode (Toml.table mpdCodec "mpd") toml
    fromToml parsed
  where
    fromToml toml'
        | not (enabled toml') = Left Disabled
        | not (isValid (musicDirectory toml')) = Left InvalidPath
        | otherwise = Right $ MpdConfig
            { mpdEnabled        = enabled toml'
            , mpdMusicDirectory = musicDirectory toml'
            , mpdCoverName      = coverName toml'
            }

-- | MPD configuration options required by the application
data MpdConfig = MpdConfig
    { mpdEnabled :: Bool
    , mpdMusicDirectory :: FilePath
    , mpdCoverName :: String
    } deriving (Show)

-- | MPD configuration options as they can be read from the TOML configuration file
data TomlMpdConfig = TomlMpdConfig
    { enabled :: Bool
    , musicDirectory :: FilePath
    , coverName :: String
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
