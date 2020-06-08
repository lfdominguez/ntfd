module Config.Weather
    ( loadWeatherConfig
    , WeatherConfig(..)
    , WeatherCfgError(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (secondsToNominalDiffTime, NominalDiffTime)
import Data.ByteString (ByteString)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec, DecodeException)
import qualified Toml

import Config.Env (loadSecret)
import Config.Global (GlobalConfig)

-- | Load weather configuration from raw TOML content
loadWeatherConfig :: Text -> GlobalConfig -> IO (Either WeatherCfgError WeatherConfig)
loadWeatherConfig toml global = do
    let decoded = decode (Toml.table weatherCodec "openweathermap") toml
    case first ParseError decoded of
        Left  e      -> pure $ Left e
        Right parsed -> validate parsed
  where
    validate parsed = do
        apiKey <- loadSecret $ apiKeySrc parsed
        if not (enabled parsed) -- ignore disabled module
            then pure $ Left Disabled
            else pure $ build apiKey parsed
    build apiKey parsed = case apiKey of
        Nothing  -> Left MissingApiKey
        Just key -> Right WeatherConfig
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

data WeatherCfgError
    = Disabled
    | MissingApiKey
    | ParseError DecodeException
    deriving (Show, Eq)

-- brittany-disable-next-binding
weatherCodec :: TomlCodec TomlWeatherConfig
weatherCodec = TomlWeatherConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.text "api_key" .= apiKeySrc
    <*> Toml.text "city_id" .= cityId
    <*> Toml.text "notification_body" .= notifBody
    <*> Toml.natural "sync_frequency" .= syncFrequency
    <*> Toml.text "display" .= template
