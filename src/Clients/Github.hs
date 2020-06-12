module Clients.Github
    ( fetchNotifications
    , parseBytes
    , RestNotification(..)
    , NotificationType(..)
    , Error(..)
    )
where

import Control.Exception (try)
import Data.Aeson ((.:), eitherDecode, withArray, withObject, Value(..))
import Data.Aeson.Types (parseEither, Parser)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Simple
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
    baseRequest <- parseRequest "GET https://api.github.com/notifications"
    let
        request = setRequestHeader "Authorization" authHeader
            $ setRequestHeader "User-Agent" userAgentHeader baseRequest
    res <- try $ httpLbs request
    pure $ case res of
        Left  e -> Left $ Client e
        Right r -> parseBytes $ getResponseBody r
  where
    authHeader      = ["token " <> githubApiKey cfg]
    userAgentHeader = ["ntfd"]

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
    | Client HttpException -- ^ Other type of errors returned by the underlying client.
    deriving (Show)
