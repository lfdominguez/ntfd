module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)
    , module Config.Twitch
    , module Config.Weather
    , module Config.Mpd
    )
where

import Control.Exception (try)
import qualified Data.Text.IO as TIO

import Config.Env (loadEnvironment)
import Config.Twitch
import Config.Weather
import Config.Mpd

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either TwitchCfgError TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either WeatherCfgError WeatherConfig -- ^ OpenWeatherMap configuration options
    , mpdCfg :: Either MpdCfgError MpdConfig -- ^ MPD configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either IOError Config)
loadConfig path = do
    env     <- loadEnvironment
    readRes <- try $ TIO.readFile path
    pure $ builder env <$> readRes
  where
    builder env content = Config
        { twitchCfg  = loadTwitchConfig content
        , weatherCfg = loadWeatherConfig env content
        , mpdCfg     = loadMpdConfig content
        }

data ConfigError
    = WeatherConfigErr WeatherCfgError
    | MpdConfigErr MpdCfgError
    | TwitchConfigErr TwitchCfgError
    deriving (Show)
