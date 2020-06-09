module Config.Twitch
    ( loadTwitchConfig
    , TwitchConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Toml ((.=), decode, TomlCodec)
import qualified Toml

import Config.Error (ConfigError(..))

loadTwitchConfig :: Text -> Either ConfigError TwitchConfig
loadTwitchConfig toml = first ParseError $ decode (Toml.table twitchCodec "twitch") toml

-- | Twitch configuration options required by the application
data TwitchConfig = TwitchConfig
    { twitchEnabled :: Bool
    , twitchClientId :: Text
    } deriving (Show)

-- brittany-disable-next-binding
twitchCodec :: TomlCodec TwitchConfig
twitchCodec = TwitchConfig
    <$> Toml.bool "enabled" .= twitchEnabled
    <*> Toml.text "client_id" .= twitchClientId
