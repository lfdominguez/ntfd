module Config.Global
    ( loadGlobalConfig
    , GlobalConfig(..)
    , GlobalCfgError
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Toml ((.=), decode, TomlCodec, DecodeException)
import qualified Toml

loadGlobalConfig :: Text -> Either GlobalCfgError GlobalConfig
loadGlobalConfig toml = first ParseError $ decode (Toml.table globalCodec "global") toml

-- | Global configuration options required by the application
data GlobalConfig = GlobalConfig
    { notificationTimeout :: Int
    } deriving (Show)

newtype GlobalCfgError = ParseError DecodeException deriving (Show)

-- brittany-disable-next-binding
globalCodec :: TomlCodec GlobalConfig
globalCodec = GlobalConfig
    <$> Toml.int "notification_timeout" .= notificationTimeout
