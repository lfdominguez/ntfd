module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)

    -- Global config
    , G.loadGlobalConfig
    , G.GlobalConfig(..)
    , G.GlobalCfgError

    -- Twitch config
    , T.loadTwitchConfig
    , T.TwitchConfig(..)
    , T.TwitchCfgError

    -- Weather config
    , W.loadWeatherConfig
    , W.WeatherConfig(..)
    , W.WeatherCfgError

    -- MPD config
    , M.loadMpdConfig
    , M.MpdConfig(..)
    , M.MpdCfgError
    )
where

import Control.Exception (try, IOException)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO

import qualified Config.Global as G
import qualified Config.Twitch as T
import qualified Config.Weather as W
import qualified Config.Mpd as M

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either T.TwitchCfgError T.TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either W.WeatherCfgError W.WeatherConfig -- ^ OpenWeatherMap configuration options
    , mpdCfg :: Either M.MpdCfgError M.MpdConfig -- ^ MPD configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = do
    readRes <- first IOError <$> try (TIO.readFile path)
    let globalRes = G.loadGlobalConfig <$> readRes
    case (readRes, globalRes) of
        (Right content, Right (Right global)) -> builder content global
        (Left  e      , _                   ) -> pure $ Left e
        (_            , Left e              ) -> pure $ Left e
        (_            , Right (Left e)      ) -> pure $ Left $ GlobalCfgError e
  where
    builder content global = do
        let twitchCfg  = T.loadTwitchConfig content
        let mpdCfg     = M.loadMpdConfig content global
        weatherCfg <- W.loadWeatherConfig content global
        pure $ Right Config { .. }

data ConfigError
    = IOError IOException
    | GlobalCfgError G.GlobalCfgError
    | WeatherCfgError W.WeatherCfgError
    | MpdCfgError M.MpdCfgError
    | TwitchCfgError T.TwitchCfgError
    deriving (Show)
