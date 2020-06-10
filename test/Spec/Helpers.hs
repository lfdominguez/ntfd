module Spec.Helpers where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Time.Clock (secondsToNominalDiffTime)
import System.Environment (lookupEnv)

import Config (Config(..))
import Config.Mpd (MpdConfig(..))
import Config.Github (GithubConfig(..))
import Config.Weather (WeatherConfig(..))
import Config.Twitch (TwitchConfig(..))

defaultCfg :: IO Config
defaultCfg = do
    weather <- defaultWeatherCfg
    github  <- defaultGithubCfg
    twitch  <- defaultTwitchCfg
    pure Config
        { weatherCfg = Right weather
        , githubCfg  = Right github
        , twitchCfg  = Right twitch
        , mpdCfg     = Right defaultMpdCfg
        }

defaultMpdCfg :: MpdConfig
defaultMpdCfg = MpdConfig
    { mpdEnabled          = True
    , mpdMusicDirectory   = "/home/musicguy/collection"
    , mpdNotifTimeout     = secondsToNominalDiffTime 10
    , mpdCoverName        = "cover.jpg"
    , mpdSkipMissingCover = True
    }

defaultWeatherCfg :: IO WeatherConfig
defaultWeatherCfg = do
    apiKey <- lookupEnv "OWM_API_KEY"
    pure WeatherConfig
        { weatherEnabled      = True
        , weatherApiKey       = fromJust $ pack <$> apiKey
        , weatherCityId       = "6077243"
        , weatherNotifTimeout = secondsToNominalDiffTime 10
        , weatherNotifBody    = "hullo"
        , weatherSyncFreq     = secondsToNominalDiffTime 1800
        , weatherTemplate     =
            "{{ temp_icon }} {{ temp_celcius }}°C {{ trend }} {{ forecast_icon }} {{ forecast_celcius }}°C"
        }

defaultGithubCfg :: IO GithubConfig
defaultGithubCfg = do
    apiKey <- lookupEnv "GITHUB_API_KEY"
    pure GithubConfig
        { githubEnabled    = True
        , githubApiKey     = fromJust $ pack <$> apiKey
        , githubNotifTime  = secondsToNominalDiffTime 10
        , githubShowAvatar = True
        , githubSyncFreq   = secondsToNominalDiffTime 30
        , githubTemplate   = "{{ notification_count }}"
        , githubAvatarDir  = "/home/someone/.cache/ntfd/github_avatar"
        }

defaultTwitchCfg :: IO TwitchConfig
defaultTwitchCfg = do
    pure TwitchConfig { twitchEnabled = True, twitchClientId = "3szce2tmzmjbvug7x3fkiwhe5xgk3f" }
