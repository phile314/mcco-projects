-- | This module tests the 'Bibtex' 'Tree' instance.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bibtex
import BibHtml.Spec
import BibHtml.Converter
import CCO.Feedback (runFeedback)
import Data.Maybe (isJust)
import Data.List (lookup) 
import Html.Tree (HtmlTree)

import System.Exit (exitFailure)
import System.IO (stderr)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Test.QuickCheck.Monadic
import Utility

-- | Returns the expected result when running the AG using the given
-- specification tree and bibtex entry.
expected :: BibtexEntry -> SpecTree -> [HtmlTree]
expected e s = html_Syn_SpecTree $ walkTree fieldToHtml e s

prop_either :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_either e s1 s2 = lbl [s1, s2] $ monadicIO $ wp m test
  where m = runFeedback (entryToHtml e) 1 1 stderr
        t = Either s1 s2    -- FIX Should be generated directly with a generator
        test Nothing = error "Validation failed"
        test (Just actual) = assert $ 
          (isAvail e t) == (isAvail e s1 || isAvail e s2)
          &&
          case (isAvail e s1, isAvail e s2) of
            (True, _)      ->  actual == expected e s1
            (False, True)  ->  actual == expected e s2
            (False, False) -> error "Computation should already be aborted" 

prop_both :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_both e s1 s2 = lbl [s1, s2] $ monadicIO $ wp m test
  where m = runFeedback (entryToHtml e) 1 1 stderr
        t = (Both s1 s2)  -- FIX Should be generated directly with a generator
        test Nothing = error "Validation failed"
        test (Just actual) = assert $ ((isAvail e t) == (isAvail e s1 && isAvail e s2))
                      && (actual == (expected e s1) ++ (expected e s2)) -- Not sure about this

prop_optional :: BibtexEntry -> SpecTree -> Property
prop_optional e s = lbl [s] $ monadicIO $ wp m test
  where m = runFeedback (entryToHtml e) 1 1 stderr
        test Nothing = error "Validation failed"
        test (Just actual) = assert $ (isAvail e (Optional s) == True) && (
          case isAvail e (Optional s) of
            True  -> actual == expected e (Optional s)
            False -> actual == expected e s)

prop_exactly :: BibtexEntry -> Field -> Property
prop_exactly e@(Entry _ _ fs) f = property $
    isAvail e (Exactly f) == (isJust $ lookup f fs)


-- | The entry point of the test suite
main :: IO ()
main = do 
  r1 <- quickCheckResult prop_either
  r2 <- quickCheckResult prop_both
  r3 <- quickCheckResult prop_optional
  r4 <- quickCheckResult prop_exactly
  if passQ [r1, r2, r3, r4]
    then return ()
    else exitFailure

depth (Both a b)    = 1 + (max (depth a) (depth b))
depth (Either a b)  = 1 + (max (depth a) (depth b))
depth (Optional a)  = 1 + (depth a)
depth (Exactly _)   = 1

lbl ss c = label ("Tree depth: " ++ (show $ maximum $ map depth ss)) c
