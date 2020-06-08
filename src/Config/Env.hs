module Config.Env
    ( loadSecret
    , expandPath
    , Env(..)
    )
where

import Data.Text (breakOnEnd, pack, unpack, Text)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath (joinPath)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T (readFile)

-- | Load a secret configuration option such as:
-- env:MY_ENV_VARIABLE
-- file:/path/to/secret
loadSecret :: Text -> IO (Maybe Text)
loadSecret input = case breakOnEnd ":" input of
    ("env:" , envVar) -> fromEnv envVar
    ("file:", file  ) -> fromFile file
    _                 -> pure $ Just input
  where
    fromEnv env = do
        variable <- lookupEnv $ unpack env
        pure $ pack <$> variable
    fromFile file = do
        path   <- expandPath $ unpack file
        exists <- doesFileExist path
        if exists -- read the first line or return nothing
            then firstLine . T.lines <$> T.readFile path
            else pure Nothing
    firstLine (h : _) = Just h
    firstLine []      = Nothing

-- | Expand path a relative path to an absolute one
expandPath :: FilePath -> IO FilePath
expandPath ('~' : '/' : p) = do
    home <- getHomeDirectory
    pure $ joinPath [home, p]
expandPath p = pure p

-- | Environment variables potentially defined
data Env = Env
    { weatherApiKey :: Maybe Text
    , configFilePath :: Maybe Text
    }

