module Spec.Clients.Github where

import Data.Either (isRight)
import Test.Hspec
import qualified Data.ByteString.Lazy as B

import Spec.Helpers (defaultGithubCfg)
import Clients.Github (fetchNotifications, parseBytes, NotificationType(..), RestNotification(..))

spec :: IO ()
spec = hspec $ describe "Github client" $ do
    it "should parse bytestring successfuly" $ do
        bytes <- B.readFile "test/samples/new_github_event.json"
        let Right [parsed] = parseBytes bytes
        isNew parsed `shouldBe` True
        title parsed `shouldBe` "Remove pgp_uuid"
        notificationType parsed `shouldBe` PullRequest
        repoName parsed `shouldBe` "graphql-ap-engine"
        avatarUrl parsed `shouldBe` "https://avatars2.githubusercontent.com/u/22077455?v=4"

    it "should fetch notifications from the API" $ do
        config        <- defaultGithubCfg
        notifications <- fetchNotifications config
        notifications `shouldSatisfy` isRight
