module Config.Global
    ( loadGlobalConfig
    , GlobalConfig(..)
    , GlobalCfgError(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.Clock (secondsToNominalDiffTime, NominalDiffTime)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec, DecodeException)
import qualified Toml

loadGlobalConfig :: Text -> Either GlobalCfgError GlobalConfig
loadGlobalConfig toml = do
    parsed <- first ParseError $ decode (Toml.table globalCodec "global") toml
    pure $ GlobalConfig { notificationTimeout = toDiffTime (timeout parsed) }
    where toDiffTime = secondsToNominalDiffTime . fromInteger . toInteger

{- HLINT ignore "Use newtype instead of data" -}
-- | Global configuration options required by the application
data GlobalConfig = GlobalConfig
    { notificationTimeout :: NominalDiffTime
    } deriving (Show)

-- | OpenWeatherMap configuration options as they can be read from the TOML configuration file
data TomlGlobalConfig = TomlGlobalConfig
    { timeout :: Natural
    }

newtype GlobalCfgError = ParseError DecodeException deriving (Show)

globalCodec :: TomlCodec TomlGlobalConfig
globalCodec = TomlGlobalConfig <$> Toml.natural "notification_timeout" .= timeout
