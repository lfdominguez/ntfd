module Stores.Github
    ( Store(..)
    , Error(..)
    , GithubClient
    )
where

import Control.Concurrent (newEmptyMVar, tryReadMVar, tryTakeMVar, putMVar, MVar)
import Data.Aeson ((.=), object)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Lazy (toStrict, fromStrict)
import Numeric.Natural (Natural)
import Text.Microstache (compileMustacheText, renderMustacheW, MustacheWarning)
import Text.Parsec (ParseError)

import Config (GithubConfig(..))

-- | Query and update Github data.
class Store s where
    -- | Initialize the store
    initGithubStore :: GithubConfig -> IO s

-- Main client, implements the Store typeclass
data GithubClient = GithubClient
    { config :: GithubConfig
    , internalState :: MVar InternalState
    }

-- Holds internal state: rendered template and github data from OWM
data InternalState = InternalState
    { githubData :: GithubData
    , renderedTemplate :: Either Error Text
    }

-- Actual github data
data GithubData = GithubData
    { unreadNotifications :: Natural
    , notifications :: [GithubNotification]
    }

-- A Github notification
data GithubNotification = GithubNotification
    { notificationId :: Text
    , isNew :: Bool
    , title :: Text
    }

instance Store GithubClient where
    initGithubStore config = do
        internalState <- newEmptyMVar
        pure GithubClient { .. }

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
        [ "notification_count" .= (notifCount :: Text)
        ]
    notifCount = undefined

-- | Error types the store might return.
data Error
    = Parse ParseError
    | Render [MustacheWarning]
    | Unsynchronized
    -- Owm [Owm.Error]
    deriving (Show)
