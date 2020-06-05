module Config.Weather
    ( loadWeatherConfig
    , WeatherConfig(..)
    , WeatherCfgError
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

import Config.Env (Env)
import qualified Config.Env as E

-- | Load weather configuration from raw TOML content
loadWeatherConfig :: Env -> Text -> Either WeatherCfgError WeatherConfig
loadWeatherConfig env toml = do
    parsed <- first ParseError $ decode (Toml.table weatherCodec "openweathermap") toml
    applyEnvFallbacks env parsed

-- Apply environment variable fallbacks to build the final configuration
applyEnvFallbacks :: Env -> TomlWeatherConfig -> Either WeatherCfgError WeatherConfig
applyEnvFallbacks env toml = case (E.weatherApiKey env, apiKey toml) of
    (Just k, _     ) -> withKey k toml
    (_     , Just k) -> withKey k toml
    _                -> Left MissingApiKey
  where
    withKey k toml'
        | not (enabled toml') = Left Disabled
        | otherwise = Right $ WeatherConfig
            { weatherEnabled   = enabled toml'
            , weatherApiKey    = encodeUtf8 k
            , weatherCityId    = encodeUtf8 $ cityId toml'
            , weatherNotifBody = notifBody toml'
            , weatherSyncFreq  = toDiffTime $ syncFrequency toml'
            , weatherTemplate  = template toml'
            }
    toDiffTime = secondsToNominalDiffTime . fromInteger . toInteger

-- | OpenWeatherMap configuration options required by the application
data WeatherConfig = WeatherConfig
    { weatherEnabled :: Bool
    , weatherApiKey :: ByteString
    , weatherCityId :: ByteString
    , weatherNotifBody :: Text
    , weatherSyncFreq :: NominalDiffTime
    , weatherTemplate :: Text
    } deriving (Show)

-- | OpenWeatherMap configuration options as they can be read from the TOML configuration file
data TomlWeatherConfig = TomlWeatherConfig
    { enabled :: Bool
    , apiKey :: Maybe Text -- ^ Optional in the config file, can be specified via env variables
    , cityId :: Text
    , notifBody :: Text
    , syncFrequency :: Natural
    , template :: Text
    }

data WeatherCfgError
    = Disabled
    | MissingApiKey
    | ParseError DecodeException
    deriving (Show)

-- brittany-disable-next-binding
weatherCodec :: TomlCodec TomlWeatherConfig
weatherCodec = TomlWeatherConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.dioptional (Toml.text "api_key") .= apiKey
    <*> Toml.text "city_id" .= cityId
    <*> Toml.text "notification_body" .= notifBody
    <*> Toml.natural "sync_frequency" .= syncFrequency
    <*> Toml.text "display" .= template
