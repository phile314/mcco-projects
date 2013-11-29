-- | Provides functions to get command line options.
module Options
    ( useSaneSerialize )
where

import System.Environment

-- | Returns whether we should use show/read serialization instead of the cco parser/printer functions.
useSaneSerialize :: IO Bool
useSaneSerialize = do
    s <- getArgs
    case s of
        ("--sane":_) -> return True
        _            -> return False
