module Spec.Config where

import Data.Maybe (isJust)
import Data.Text (breakOnEnd, pack)
import Test.Hspec

import Config (loadConfig, Config(..), MpdConfig(..), WeatherConfig(..))
import Config.Env (expandPath, loadSecret)
import qualified Config.Mpd as MErr (MpdCfgError(..))
import qualified Config.Weather as WErr (WeatherCfgError(..))

spec :: IO ()
spec = hspec $ describe "Configuration" $ do
    it "should read default config file (everything disabled)" $ do
        toml <- loadConfig "config.toml"
        let Right config           = toml

        let Left  weatherConfigErr = weatherCfg config
        weatherConfigErr `shouldBe` WErr.Disabled

        let Left mpdConfigErr = mpdCfg config
        mpdConfigErr `shouldBe` MErr.Disabled

    it "should read test config file (everything enabled)" $ do
        toml <- loadConfig "test/Spec/test-config.toml"
        let Right config        = toml

        let Right weatherConfig = weatherCfg config
        let weatherOn           = weatherEnabled weatherConfig
        let cityId              = weatherCityId weatherConfig
        weatherOn `shouldBe` True
        cityId `shouldBe` "6077243"

        let Right mpdConfig = mpdCfg config
        let mpdOn           = mpdEnabled mpdConfig
        let mpdCover        = mpdCoverName mpdConfig
        mpdOn `shouldBe` True
        mpdCover `shouldBe` "cover.jpg"

    it "should expand file paths correctly" $ do
        homePath <- expandPath "~/some_file.txt"
        let (base, _) = breakOnEnd "/home" $ pack homePath
        base `shouldBe` "/home"

        let full = "/root/some_file.txt"
        fullPath <- expandPath full
        full `shouldBe` fullPath

    it "should load secrets from different sources" $ do
        secretFile <- loadSecret "file:test/Spec/test-token.txt"
        secretFile `shouldBe` Just "some-secret-token"

        secretEnv <- loadSecret "env:OWM_API_KEY"
        secretEnv `shouldSatisfy` isJust

        rawSecret <- loadSecret "not very well hidden"
        rawSecret `shouldBe` Just "not very well hidden"
