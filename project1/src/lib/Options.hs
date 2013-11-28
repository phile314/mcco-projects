module Options
    ( useSaneSerialize )
where

import System.Environment

useSaneSerialize :: IO Bool
useSaneSerialize = do
    s <- getArgs
    case s of
        ("--sane":_) -> return True
        _            -> return False
