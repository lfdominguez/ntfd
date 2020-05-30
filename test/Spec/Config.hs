module Spec.Config where

import Test.Hspec

import Config (loadConfig, Config(..), TwitchConfig(..), WeatherConfig(..))

spec :: IO ()
spec = hspec $ describe "Configuration" $ it "should read example config file" $ do
    toml <- loadConfig "config.toml"
    let Right config       = toml

    let Right twitchConfig = twitchCfg config
    let twitchOn           = twitchEnabled twitchConfig
    twitchOn `shouldBe` True

    let Right weatherConfig = weatherCfg config
    let weatherOn           = weatherEnabled weatherConfig
    let cityId              = weatherCityId weatherConfig
    weatherOn `shouldBe` True
    cityId `shouldBe` "6077243"

