module Config.Weather
    ( loadWeatherConfig
    , WeatherConfig(..)
    , WeatherCfgError
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
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
    (Just k, _     ) -> Right $ withKey k toml
    (_     , Just k) -> Right $ withKey k toml
    _                -> Left MissingApiKey
  where
    withKey k toml' = WeatherConfig
        { weatherEnabled  = enabled toml'
        , weatherApiKey   = encodeUtf8 k
        , weatherCityId   = encodeUtf8 $ cityId toml'
        , weatherTemplate = template toml'
        }

-- | OpenWeatherMap configuration options required by the application
data WeatherConfig = WeatherConfig
    { weatherEnabled :: Bool
    , weatherApiKey :: ByteString
    , weatherCityId :: ByteString
    , weatherTemplate :: Text
    } deriving (Show)

-- | OpenWeatherMap configuration options as they can be read from the TOML configuration file
data TomlWeatherConfig = TomlWeatherConfig
    { enabled :: Bool
    , apiKey :: Maybe Text -- ^ Optional in the config file, can be specified via env variables
    , cityId :: Text
    , template :: Text
    }

data WeatherCfgError
    = MissingApiKey
    | ParseError DecodeException
    deriving (Show)

-- brittany-disable-next-binding
weatherCodec :: TomlCodec TomlWeatherConfig
weatherCodec = TomlWeatherConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.dioptional (Toml.text "api_key") .= apiKey
    <*> Toml.text "city_id" .= cityId
    <*> Toml.text "display" .= template
