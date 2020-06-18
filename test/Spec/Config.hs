module Spec.Config where

import Data.Maybe (isJust)
import Data.Text (breakOn, breakOnEnd, pack)
import Test.Hspec

import Config
    (loadConfig, Config(..), MpdConfig(..), GithubConfig(..), WeatherConfig(..), ConfigError(..))
import Config.Env (expandPath, loadSecret)

spec :: IO ()
spec = hspec $ describe "Configuration" $ do
    it "should read default config file (everything disabled)" $ do
        toml <- loadConfig "config.toml"
        let Right config           = toml

        let Left  weatherConfigErr = weatherCfg config
        weatherConfigErr `shouldBe` Disabled

        let Left mpdConfigErr = mpdCfg config
        mpdConfigErr `shouldBe` Disabled

    it "should read test config file (everything enabled)" $ do
        toml <- loadConfig "test/samples/test_config.toml"
        let Right config        = toml

        let Right weatherConfig = weatherCfg config
        let weatherOn           = weatherEnabled weatherConfig
        let cityId              = weatherCityId weatherConfig
        weatherOn `shouldBe` True
        cityId `shouldBe` "6077243"

        let Right githubConfig = githubCfg config
        let githubOn           = githubEnabled githubConfig
        let withAvatars        = githubShowAvatar githubConfig
        let (_, cacheDir) = breakOn ".cache/ntfd" $ pack $ githubAvatarDir githubConfig
        githubOn `shouldBe` True
        withAvatars `shouldBe` True
        cacheDir `shouldBe` ".cache/ntfd/github_avatars"

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
        secretFile <- loadSecret "file:test/samples/test_github_token.txt"
        secretFile `shouldBe` Just "some-secret-token"

        secretEnv <- loadSecret "env:OWM_API_KEY"
        secretEnv `shouldSatisfy` isJust

        rawSecret <- loadSecret "not very well hidden"
        rawSecret `shouldBe` Just "not very well hidden"
