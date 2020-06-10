module Config.Github
    ( loadGithubConfig
    , GithubConfig(..)
    )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime)
import Data.ByteString (ByteString)
import Numeric.Natural (Natural)
import Toml ((.=), decode, TomlCodec)
import qualified Toml

import Config.Env (expandPath, loadSecret)
import Config.Error (ConfigError(..))
import Helpers (normalizeDuration, toDiffTime)

-- | Load github configuration from raw TOML content
loadGithubConfig :: Text -> IO (Either ConfigError GithubConfig)
loadGithubConfig toml = do
    let decoded = decode (Toml.table githubCodec "github") toml
    case first ParseError decoded of
        Left  e      -> pure $ Left e
        Right parsed -> withEnv parsed
  where
    withEnv parsed = do
        apiKey   <- loadSecret $ apiKeySrc parsed
        cacheDir <- expandPath "~/.cache/ntfd/github_avatars"
        pure $ build apiKey cacheDir parsed
    build Nothing _ _ = Left MissingApiKey
    build (Just key) cacheDir parsed
        | not (enabled parsed) = Left Disabled
        | otherwise = Right GithubConfig
            { githubEnabled    = enabled parsed
            , githubApiKey     = encodeUtf8 key
            , githubNotifTime  = toDiffTime $ notifTimeout parsed
            , githubShowAvatar = showAvatar parsed
            , githubSyncFreq   = normalizeDuration 10 $ syncFrequency parsed
            , githubTemplate   = template parsed
            , githubAvatarDir  = cacheDir
            }

-- | Github configuration options required by the application
data GithubConfig = GithubConfig
    { githubEnabled :: Bool
    , githubApiKey :: ByteString
    , githubNotifTime :: NominalDiffTime
    , githubShowAvatar :: Bool
    , githubSyncFreq :: NominalDiffTime
    , githubTemplate :: Text
    , githubAvatarDir :: FilePath
    } deriving (Show)

-- | Github configuration options as they can be read from the TOML configuration file
data TomlGithubConfig = TomlGithubConfig
    { enabled :: Bool
    , apiKeySrc :: Text
    , notifTimeout :: Natural
    , showAvatar :: Bool
    , syncFrequency :: Natural
    , template :: Text
    }

-- brittany-disable-next-binding
githubCodec :: TomlCodec TomlGithubConfig
githubCodec = TomlGithubConfig
    <$> Toml.bool "enabled" .= enabled
    <*> Toml.text "api_key" .= apiKeySrc
    <*> Toml.natural "notification_timeout" .= notifTimeout
    <*> Toml.bool "show_avatar" .= showAvatar
    <*> Toml.natural "sync_frequency" .= syncFrequency
    <*> Toml.text "display" .= template
