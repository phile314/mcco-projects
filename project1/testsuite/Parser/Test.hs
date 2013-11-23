-- | This module contains parser tests for test-driven-development

module Main where

import System.Exit (exitFailure)
import Utility
import qualified ParserHUnit as H
import qualified ParserQuick as Q
import Test.HUnit
import Test.QuickCheck

-- | The entry point of the test suite
main :: IO ()
main = do  
  successH <- runTestTT H.tests >>= return . passH
  successQ <- mapM quickCheckResult Q.tests >>= return . passQ
  if (successH && successQ)  
    then return ()
    else exitFailure
