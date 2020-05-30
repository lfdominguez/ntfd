module Stores.Twitch
    ( Store(..)
    , Stream(..)
    , Error(..)
    )
where

import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)

import Config (TwitchConfig)

-- | Query and update Twitch data for the current user.
class Store s where
    -- | Initialize a Twitch store, should also perform required authentication.
    init :: TwitchConfig -> IO (Either (Error e) s)
    -- | Get the number of live stream the current user is following.
    liveStreamCount :: s -> Maybe Int
    -- | Get live streams metadata.
    liveStreams :: s -> Maybe [Stream]
    -- | Update live streams metadata.
    updateLiveStreams :: s -> IO (Either (Error e) ())

-- | Metadata for a given Twitch stream.
data Stream = Stream
    { streamerName :: Text
    , message :: Text
    , gameName :: Text
    , currentViewers :: Int
    , streamDuration :: NominalDiffTime
    }

-- | Error types the  store might return.
data Error e
    = Init Text -- ^ Store failed to initialize, likely due to authentication errors.
    | Parse e -- ^ Failed to parse a response from the Twitch API.
    | Client e -- ^ Other type of errors returned by the underlying client.
