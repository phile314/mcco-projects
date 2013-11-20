-- | This module tests the 'Bibtex' 'Tree' instance.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bibtex
import CCO.Tree (toTree, fromTree, Tree, ATerm)
import CCO.Feedback
import Data.Maybe (fromJust)
import System.IO
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Test.QuickCheck.Monadic
import Utility

-- | Checks that @'fromTree' . 'toTree'@ is the identity function.
prop_from_to_identity :: (Eq a, Tree a) => a -> Property
prop_from_to_identity expected = monadicIO $ wp m test
  where m = runFeedback ((toTree . fromTree) expected) 1 1 stderr
        test (Just actual) = assert (actual == expected)
        test Nothing = error "toTree failed"

-- | Checks that @'toTree' . 'fromTree'@ is the identity function.
-- Note that the input is supposed to be a valid 'ATerm' representing 
-- something of type a.
prop_to_from_identity :: (Tree a, Eq a) 
                      => a -- In order to make the compiler choose the correct instance of Tree
                      -> ATerm -> Property
prop_to_from_identity r' expected =  monadicIO $ wp m test
  where m = runFeedback (toTree expected) 1 1 stderr
        test (Just r) = assert (fromTree (r `asTypeOf` r') == expected) -- forces to deduce that r' has type a
        test Nothing = error "toTree failed"

-- | Tests 'prop_from_to_identity' for 'BibtexEntry'.
prop_identity_entry :: BibtexEntry -> Property
prop_identity_entry = prop_from_to_identity

-- | Tests 'prop_from_to_identity' for 'BibtexDb'.
prop_identity_db :: BibtexDb -> Property
prop_identity_db = prop_from_to_identity

-- | Tests 'prop_to_from_identity' for the bibtex type a.
identityBibtex :: (Eq a, Tree a) => a -> ATerm -> Property
identityBibtex a = prop_to_from_identity a

-- | Tests to - from for the type 'BibtexEntry'
prop_identity_entry' :: ATerm -> Property
prop_identity_entry' = identityBibtex (undefined :: BibtexEntry)

-- | Tests to - from for the type 'BibtexDb'
prop_identity_db' :: ATerm -> Property
prop_identity_db' = identityBibtex (undefined :: BibtexDb)

-- | The entry point of the test suite
main :: IO ()
main = do 
  r1 <- quickCheckResult prop_identity_entry
  r2 <- quickCheckResult prop_identity_db
  r3 <- quickCheckResult (forAll bibtexEntryATerm prop_identity_entry')
  r4 <- quickCheckResult (forAll bibtexDbATerm prop_identity_db')
  if all isSuccess [r1,r2, r3, r4]
    then return ()
    else exitFailure
