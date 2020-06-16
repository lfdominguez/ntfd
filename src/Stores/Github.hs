module Stores.Github
    ( Store(..)
    , Error(..)
    , GithubClient
    )
where

import Control.Concurrent (newEmptyMVar, tryReadMVar, tryTakeMVar, putMVar, MVar)
import Data.Aeson ((.=), object)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Numeric.Natural (Natural)
import Text.Microstache (compileMustacheText, renderMustacheW, MustacheWarning)
import Text.Parsec (ParseError)

import Clients.Github (fetchNotifications, RestNotification(..), NotificationType)
import Config (GithubConfig(..))
import qualified Clients.Github as Gh

-- | Query and update Github data.
class Store s where
    -- | Initialize the store
    initGithubStore :: GithubConfig -> IO s
    -- | Synchronize local data with the Github API.
    -- Boolean value can be used to know whether or not notifications should be sent.
    syncGithub :: s ->  IO (Either Error Bool)
    -- | Get a rendered version of the configured template
    getRenderedTemplate :: s -> IO (Either Error Text)

-- Main client, implements the Store typeclass
data GithubClient = GithubClient
    { config :: GithubConfig
    , internalState :: MVar InternalState
    }

-- Holds internal state: rendered template and notification data from Github
data InternalState = InternalState
    { githubData :: GithubData
    , renderedTemplate :: Either Error Text
    }

-- Actual github data
data GithubData = GithubData
    { unreadNotifications :: Natural
    , notifications :: [GithubNotification]
    }

-- A Github notification, adds properties on top of the raw Rest structure
data GithubNotification = GithubNotification
    { restNotification :: RestNotification
    , avatarPath :: FilePath
    } deriving (Show)

instance Store GithubClient where
    initGithubStore config = do
        internalState <- newEmptyMVar
        pure GithubClient { .. }

    syncGithub s = do
        notifs <- fetchNotifications cfg
        case notifs of
            Right restNotifs -> do
                -- Build new state
                let unreadNotifications = unreadCount
                    notifications       = toCompleteNotif restNotifs
                    gData               = GithubData { .. }
                    renderedTemplate    = renderTemplate gData template
                    state               = InternalState { githubData = gData, .. }
                -- Update store
                oldState <- tryTakeMVar mvar
                putMVar mvar state
                pure $ Right $ shouldNotify (githubData <$> oldState)
            -- Report failure otherwise
            Left e -> pure $ Left $ GithubApi e
      where
        cfg          = config s
        mvar         = internalState s
        template     = githubTemplate cfg
        shouldNotify = undefined
        unreadCount  = undefined
        toCompleteNotif r = undefined

    getRenderedTemplate s = do
        state <- tryReadMVar $ internalState s
        pure $ case state of
            Nothing -> Left Unsynchronized
            Just w  -> renderedTemplate w

-- brittany-disable-next-binding
renderTemplate :: GithubData -> Text -> Either Error Text
renderTemplate w t = do
    -- Compile user provided template
    template <- first Parse $ compileMustacheText "github template" $ fromStrict t
    -- Render it
    case renderMustacheW template payload of
        ([]  , res) -> Right $ toStrict res
        (errs, _  ) -> Left $ Render errs
  where
    payload = object
        [ "notification_count" .= notifCount
        ]
    notifCount = unreadNotifications w

-- | Error types the store might return.
data Error
    = Parse ParseError
    | Render [MustacheWarning]
    | Unsynchronized
    | GithubApi Gh.Error
    deriving (Show)
