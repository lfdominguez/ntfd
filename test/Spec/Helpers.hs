module Spec.Helpers where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Time.Clock (secondsToNominalDiffTime)
import System.Environment (lookupEnv)

import Config (Config(..))
import Config.Global (GlobalConfig(..))
import Config.Mpd (MpdConfig(..))
import Config.Weather (WeatherConfig(..))
import Config.Twitch (TwitchConfig(..))

defaultCfg :: IO Config
defaultCfg = do
    weather <- defaultWeatherCfg
    twitch  <- defaultTwitchCfg
    pure Config
        { weatherCfg = Right weather
        , twitchCfg  = Right twitch
        , mpdCfg     = Right defaultMpdCfg
        }

defaultGlobalCfg :: GlobalConfig
defaultGlobalCfg = GlobalConfig { notificationTimeout = secondsToNominalDiffTime 10 }

defaultMpdCfg :: MpdConfig
defaultMpdCfg = MpdConfig
    { mpdGlobalCfg        = defaultGlobalCfg
    , mpdEnabled          = True
    , mpdMusicDirectory   = "/home/musicguy/collection"
    , mpdCoverName        = "cover.jpg"
    , mpdSkipMissingCover = True
    }

defaultWeatherCfg :: IO WeatherConfig
defaultWeatherCfg = do
    apiKey <- lookupEnv "OWM_API_KEY"
    pure WeatherConfig
        { weatherGlobalCfg = defaultGlobalCfg
        , weatherEnabled   = True
        , weatherApiKey    = fromJust $ pack <$> apiKey
        , weatherCityId    = "6077243"
        , weatherNotifBody = "hullo"
        , weatherSyncFreq  = secondsToNominalDiffTime 1800
        , weatherTemplate  =
            "{{ temp_icon }} {{ temp_celcius }}°C {{ trend }} {{ forecast_icon }} {{ forecast_celcius }}°C"
        }

defaultTwitchCfg :: IO TwitchConfig
defaultTwitchCfg = do
    pure TwitchConfig { twitchEnabled = True, twitchClientId = "3szce2tmzmjbvug7x3fkiwhe5xgk3f" }
