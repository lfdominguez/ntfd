module Clients.Github
    ( fetchNotifications
    , getAvatarPath
    , parseBytes
    , RestNotification(..)
    , NotificationType(..)
    , Error(..)
    )
where

import Codec.Picture (readImage, saveJpgImage)
import Control.Exception (try, IOException)
import Data.Aeson ((.:), eitherDecode, withArray, withObject, Value(..))
import Data.Aeson.Types (parseEither, Parser)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (fromStrict, toStrict, ByteString)
import Data.Maybe (isNothing)
import Data.Text (replace, toLower, unpack, Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.Http.Client
import System.FilePath (joinPath)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Config (GithubConfig(..))

-- A Github notification fetched from the REST APIv3
data RestNotification = RestNotification
    { notificationId :: Text
    , notificationType :: NotificationType
    , isNew :: Bool
    , title :: Text
    , repoName :: Text
    , repoFullName :: Text
    , repoHtmlUrl :: Text
    , avatarUrl :: ByteString
    } deriving (Show)

-- | Type of notification received
data NotificationType
    = PullRequest
    | Issue
    | Notification -- catch all, possible types are undocumented ...
    deriving (Show, Eq)

-- Fetch Github notifications
fetchNotifications :: GithubConfig -> IO (Either Error [RestNotification])
fetchNotifications cfg = do
    ctx <- baselineContextSSL
    let connection = openConnectionSSL ctx "api.github.com" 443
    res <- try $ withConnection connection getBytes
    pure $ case res of
        Left  e -> Left $ Client e
        Right r -> parseBytes $ fromStrict r
  where
    request = buildRequest1 $ do
        http GET "/notifications"
        setAccept "application/json"
        setHeader "Authorization" ("token " <> githubApiKey cfg)
        setHeader "User-Agent"    "ntfd"
    getBytes c = do
        sendRequest c request emptyBody
        receiveResponse c concatHandler

-- Returns the local path to a github avatar, fetching and caching it if necessary
getAvatarPath :: GithubConfig -> Text -> ByteString -> IO (Either Error FilePath)
getAvatarPath cfg repoName avatarUrl = do
    let expectedPath = normalizePath
    exists <- doesFileExist expectedPath
    if exists
        then pure $ Right expectedPath
        else do
            createDirectoryIfMissing True avatarDir
            writeRes  <- first Client <$> try (fetchAvatar avatarUrl expectedPath)
            converted <- convertImage expectedPath
            pure $ writeRes >> converted >> Right expectedPath
  where
    avatarDir     = githubAvatarDir cfg
    normalizePath = joinPath [avatarDir, unpack $ normalizeName repoName]
    normalizeName n = (toLower . replace "/" "__") n <> ".jpg"
    fetchAvatar url path = do
        bytes <- get (toStrict url) concatHandler
        B.writeFile path bytes
    convertImage path = do
        let outPath = path <> "lol"
        img <- first Convert <$> readImage path
        case img of
            Right i -> saveJpgImage 10 outPath i >> pure (Right path)
            Left  e -> pure $ Left e

-- Parse the bytestring response
parseBytes :: ByteString -> Either Error [RestNotification]
parseBytes bytes = do
    value <- first InvalidJson $ eitherDecode bytes
    first Parse $ parseEither parseNotifs value

-- Parse the top level array of notification objects
parseNotifs :: Value -> Parser [RestNotification]
parseNotifs = withArray "top level notification array" $ \arr -> mapM parseNotif (V.toList arr)

-- Parse a notification object
parseNotif :: Value -> Parser RestNotification
parseNotif = withObject "notification" $ \o -> do
    subject          <- o .: "subject"
    repository       <- o .: "repository"
    owner            <- repository .: "owner"
    notificationId   <- o .: "id"
    notificationType <- toNotifType <$> (subject .: "type" :: Parser Text)
    isNew            <- isNothing <$> (o .: "last_read_at" :: Parser (Maybe Text))
    title            <- subject .: "title"
    repoName         <- repository .: "name"
    repoFullName     <- repository .: "full_name"
    repoHtmlUrl      <- owner .: "html_url"
    avatarUrl        <- encodeUtf8 <$> owner .: "avatar_url"
    return RestNotification { .. }
  where
    toNotifType t
        | t == "Issue"       = Issue
        | t == "PullRequest" = PullRequest
        | otherwise          = Notification

-- | Error types the client might return.
data Error
    = Parse String -- ^ Github responded with an unexpected JSON object.
    | InvalidJson String -- ^ Github responded with invalid JSON.
    | Client IOException -- ^ Other type of errors returned by the underlying client.
    | Convert String -- ^ Avatar conversion error
    deriving (Show)
