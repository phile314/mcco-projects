-- | This module contains a testsuite for the 'Lexer'
module Main where

import qualified LexerHUnit as H
import qualified LexerQuick as Q
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck
import Utility (passQ, passH)

-- | The entry point of the test suite
main :: IO ()
main = do  
  successH <- runTestTT H.tests >>= return . passH
  successQ <- mapM quickCheckResult Q.tests >>= return . passQ
  if (successH && successQ)  
    then return ()
    else exitFailure
