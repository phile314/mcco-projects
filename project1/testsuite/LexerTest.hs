-- | This module contains a testsuite for the 'Lexer'
module Main where

import qualified LexerHUnit as H
import qualified LexerQuick as Q
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit (exitFailure)

-- | The entry point of the test suite
main :: IO ()
main = do  
  successH <- runTestTT H.tests >>= return . passH
  successQ <- mapM quickCheckResult Q.tests >>= return . passQ
  if (successH && successQ)  
    then return ()
    else exitFailure

-- | Returns whether some 'HUnit' test failed
passH :: Counts -> Bool
passH result = failures result == 0 && errors result == 0

-- | Returns whether some 'QuickCheck' test failed
passQ :: [Result] -> Bool
passQ = all isSuccess
