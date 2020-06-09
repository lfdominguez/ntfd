module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)

    -- Global config
    , loadGlobalConfig
    , GlobalConfig(..)

    -- Twitch config
    , loadTwitchConfig
    , TwitchConfig(..)

    -- Weather config
    , loadWeatherConfig
    , WeatherConfig(..)

    -- MPD config
    , loadMpdConfig
    , MpdConfig(..)
    )
where

import Control.Exception (try)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO

import Config.Global
import Config.Twitch
import Config.Weather
import Config.Mpd
import Config.Error

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either ConfigError TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either ConfigError WeatherConfig -- ^ OpenWeatherMap configuration options
    , mpdCfg :: Either ConfigError MpdConfig -- ^ MPD configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = do
    readRes <- first IOError <$> try (TIO.readFile path)
    let globalRes = loadGlobalConfig =<< readRes
    case (readRes, globalRes) of
        (Right content, Right global) -> builder content global
        (Left  e      , _           ) -> pure $ Left e
        (_            , Left e      ) -> pure $ Left e
  where
    builder content global = do
        let twitchCfg = loadTwitchConfig content
        mpdCfg     <- loadMpdConfig content global
        weatherCfg <- loadWeatherConfig content global
        pure $ Right Config { .. }
