module Config.Error
    ( ConfigError(..)
    ) where

import Control.Exception (IOException)
import Toml (DecodeException)

data ConfigError
    = IOError IOException
    | Disabled
    | InvalidPath
    | MissingApiKey
    | ParseError DecodeException
    deriving (Show, Eq)
