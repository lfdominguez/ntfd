module Config.Weather
    ( loadWeatherConfig
    , WeatherConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (secondsToNominalDiffTime, NominalDiffTime)
import Data.ByteString (ByteString)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec)
import qualified Toml

import Config.Env (loadSecret)
import Config.Error (ConfigError(..))
import Config.Global (GlobalConfig)

-- | Load weather configuration from raw TOML content
loadWeatherConfig :: Text -> GlobalConfig -> IO (Either ConfigError WeatherConfig)
loadWeatherConfig toml global = do
    let decoded = decode (Toml.table weatherCodec "openweathermap") toml
    case first ParseError decoded of
        Left  e      -> pure $ Left e
        Right parsed -> withEnv parsed
  where
    withEnv parsed = do
        apiKey <- loadSecret $ apiKeySrc parsed
        pure $ build apiKey parsed
    build Nothing _ = Left MissingApiKey
    build (Just key) parsed
        | not (enabled parsed) = Left Disabled
        | otherwise = Right WeatherConfig
            { weatherGlobalCfg = global
            , weatherEnabled   = enabled parsed
            , weatherApiKey    = encodeUtf8 key
            , weatherCityId    = encodeUtf8 $ cityId parsed
            , weatherNotifBody = notifBody parsed
            , weatherSyncFreq  = toDiffTime $ syncFrequency parsed
            , weatherTemplate  = template parsed
            }
    toDiffTime val =
        let
            asInteger  = toInteger val
            normalized = if asInteger < 600 then 600 else asInteger
        in secondsToNominalDiffTime $ fromInteger normalized

-- | OpenWeatherMap configuration options required by the application
data WeatherConfig = WeatherConfig
    { weatherGlobalCfg :: GlobalConfig
    , weatherEnabled :: Bool
    , weatherApiKey :: ByteString
    , weatherCityId :: ByteString
    , weatherNotifBody :: Text
    , weatherSyncFreq :: NominalDiffTime
    , weatherTemplate :: Text
    } deriving (Show)

-- | OpenWeatherMap configuration options as they can be read from the TOML configuration file
data TomlWeatherConfig = TomlWeatherConfig
    { enabled :: Bool
    , apiKeySrc :: Text
    , cityId :: Text
    , notifBody :: Text
    , syncFrequency :: Natural
    , template :: Text
    }

-- brittany-disable-next-binding
weatherCodec :: TomlCodec TomlWeatherConfig
weatherCodec = TomlWeatherConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.text "api_key" .= apiKeySrc
    <*> Toml.text "city_id" .= cityId
    <*> Toml.text "notification_body" .= notifBody
    <*> Toml.natural "sync_frequency" .= syncFrequency
    <*> Toml.text "display" .= template
