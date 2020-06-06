module Config
    ( loadConfig
    , Config(..)
    , ConfigError(..)
    , module Config.Global
    , module Config.Twitch
    , module Config.Weather
    , module Config.Mpd
    )
where

import Control.Exception (try, IOException)
import Data.Bifunctor (first)
import qualified Data.Text.IO as TIO

import Config.Env (loadEnvironment)
import Config.Global
import Config.Twitch
import Config.Weather
import Config.Mpd

-- | ntfd main configuration
data Config = Config
    { twitchCfg :: Either TwitchCfgError TwitchConfig -- ^ Twitch configuration options
    , weatherCfg :: Either WeatherCfgError WeatherConfig -- ^ OpenWeatherMap configuration options
    , mpdCfg :: Either MpdCfgError MpdConfig -- ^ MPD configuration options
    } deriving (Show)

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = do
    env     <- loadEnvironment
    readRes <- first IOError <$> try (TIO.readFile path)
    let globalRes = loadGlobalConfig <$> readRes
    pure $ case (readRes, globalRes) of
        (Right content, Right (Right global)) -> Right $ builder env content global
        (Left  e      , _                   ) -> Left e
        (_            , Left e              ) -> Left e
        (_            , Right (Left e)      ) -> Left $ GlobalCfgError e
  where
    builder env content global = Config
        { twitchCfg  = loadTwitchConfig content
        , weatherCfg = loadWeatherConfig env content global
        , mpdCfg     = loadMpdConfig content global
        }

data ConfigError
    = IOError IOException
    | Kek
    | GlobalCfgError GlobalCfgError
    | WeatherCfgError WeatherCfgError
    | MpdCfgError MpdCfgError
    | TwitchCfgError TwitchCfgError
    deriving (Show)
