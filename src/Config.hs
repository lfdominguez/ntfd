module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)
    , module Config.Twitch
    , module Config.Weather
    )
where

import Control.Exception (try)
import qualified Data.Text.IO as TIO

import Config.Env (loadEnvironment)
import Config.Twitch
import Config.Weather

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either TwitchCfgError TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either WeatherCfgError WeatherConfig -- ^ OpenWeatherMap configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either IOError Config)
loadConfig path = do
    env     <- loadEnvironment
    readRes <- try $ TIO.readFile path
    pure $ builder env <$> readRes
  where
    builder env content =
        Config { twitchCfg = loadTwitchConfig content, weatherCfg = loadWeatherConfig env content }

data ConfigError
    = WeatherConfigErr WeatherCfgError
    | TwitchConfigErr TwitchCfgError
    deriving (Show)
