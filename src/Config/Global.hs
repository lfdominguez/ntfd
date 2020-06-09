module Config.Global
    ( loadGlobalConfig
    , GlobalConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.Clock (secondsToNominalDiffTime, NominalDiffTime)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec)
import qualified Toml

import Config.Error (ConfigError(..))

loadGlobalConfig :: Text -> Either ConfigError GlobalConfig
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

globalCodec :: TomlCodec TomlGlobalConfig
globalCodec = TomlGlobalConfig <$> Toml.natural "notification_timeout" .= timeout
