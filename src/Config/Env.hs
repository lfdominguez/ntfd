module Config.Env
    ( loadEnvironment
    , Env(..)
    )
where

import System.Environment (lookupEnv)
import Data.Text (pack, Text)

-- | Load environment variables
loadEnvironment :: IO Env
loadEnvironment = do
    weatherApiKey  <- textFromEnv "OWM_API_KEY"
    configFilePath <- textFromEnv "CONFIG_PATH"
    pure Env { .. }
  where
    textFromEnv k = lookupEnv k >>= stringToText
    stringToText v = pure $ pack <$> v

-- | Environment variables potentially defined
data Env = Env
    { weatherApiKey :: Maybe Text
    , configFilePath :: Maybe Text
    }

