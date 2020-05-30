module Config.Twitch
    ( loadTwitchConfig
    , TwitchConfig(..)
    , TwitchCfgError
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Toml ((.=), decode, TomlCodec, DecodeException)
import qualified Toml

loadTwitchConfig :: Text -> Either TwitchCfgError TwitchConfig
loadTwitchConfig toml = first ParseError $ decode (Toml.table twitchCodec "twitch") toml

-- | Twitch configuration options required by the application
data TwitchConfig = TwitchConfig
    { twitchEnabled :: Bool
    , twitchClientId :: Text
    } deriving (Show)

newtype TwitchCfgError = ParseError DecodeException deriving (Show)

-- brittany-disable-next-binding
twitchCodec :: TomlCodec TwitchConfig
twitchCodec = TwitchConfig
    <$> Toml.bool "enabled" .= twitchEnabled
    <*> Toml.text "client_id" .= twitchClientId
