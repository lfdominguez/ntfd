module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)

    -- Twitch config
    , loadTwitchConfig
    , TwitchConfig(..)

    -- Weather config
    , loadWeatherConfig
    , WeatherConfig(..)

    -- Github config
    , loadGithubConfig
    , GithubConfig(..)

    -- MPD config
    , loadMpdConfig
    , MpdConfig(..)
    )
where

import Control.Exception (try)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO

import Config.Github
import Config.Twitch
import Config.Weather
import Config.Mpd
import Config.Error

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either ConfigError TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either ConfigError WeatherConfig -- ^ OpenWeatherMap configuration options
    , githubCfg :: Either ConfigError GithubConfig -- ^ Github configuration options
    , mpdCfg :: Either ConfigError MpdConfig -- ^ MPD configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = do
    readRes <- first IOError <$> try (TIO.readFile path)
    case readRes of
        Right content -> builder content
        Left  e       -> pure $ Left e
  where
    builder content = do
        let twitchCfg = loadTwitchConfig content
        mpdCfg     <- loadMpdConfig content
        weatherCfg <- loadWeatherConfig content
        githubCfg  <- loadGithubConfig content
        pure $ Right Config { .. }
