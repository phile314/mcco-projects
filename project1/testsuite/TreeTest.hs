-- | This module tests the 'Bibtex' 'Tree' instance.

module Main where

import Bibtex
import CCO.Tree (toTree, fromTree)
import CCO.Feedback
import Data.Maybe (fromJust)
import System.IO
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Test.QuickCheck.Monadic
import Utility

-- | Checks that @'fromTree' . 'toTree'@ is the identity functions.
prop_from_to_identity :: BibtexEntry -> Property
prop_from_to_identity expected = monadicIO $ wp m test
  where m = runFeedback ((toTree . fromTree) expected) 1 1 stderr
        test (Just actual) = assert (actual == expected)
        test Nothing = error "toTree failed"

-- | The entry point of the test suite
main :: IO ()
main = do 
  r1 <- quickCheckResult prop_from_to_identity
  if isSuccess r1
    then return ()
    else exitFailure
