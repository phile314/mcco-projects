-- | This module tests the 'Bibtex' 'Tree' instance.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bibtex
import BibHtml.Spec
import qualified BibHtml.Spec as S
import BibHtml.Converter
import Data.Maybe (isJust)
import Data.List (lookup) 
import CCO.Feedback
import Html.Tree

import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property (succeeded)
import Utility


-- FIX Using these functions does not test that the result is correct
-- but only that some error has not been raised
msgs :: BibtexEntry -> SpecTree -> [Message]
msgs e s = getMsgs $ walkTree e s

bib :: BibtexEntry -> SpecTree -> [(Field,String)]
bib e s = getBib $ walkTree e s

-- | Asserts that either both spec are not available or both return the same tree for a given bitex entry.
assertEqualAvailHtml :: BibtexEntry -> SpecTree -> SpecTree -> Property
assertEqualAvailHtml entry act exp =
    getAvail rAct ?== getAvail rExp
    .&.
    (getAvail rExp ==> getBib rAct ?== getBib rExp)
    where rAct = walkTree entry act
          rExp = walkTree entry exp


prop_either :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_either e s1 s2 = lbl [s1, s2] $
    (isAvail e t) ?== (isAvail e s1 || isAvail e s2)
    .&.
    (isAvail e t ==>
        case (isAvail e s1, isAvail e s2) of
            (True, _)      -> assertEqualAvailHtml e t s1
            (False, True)  -> assertEqualAvailHtml e t s2
    )
    where t = S.either s1 s2

prop_both :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_both e s1 s2 = lbl [s1, s2] $
    (isAvail e t) ?== (isAvail e s1 && isAvail e s2)
    .&.
    (isAvail e t ==> (bib e t) ?== ((bib e s1) ++ (bib e s2)))
    where t = S.both s1 s2

prop_optional :: BibtexEntry -> SpecTree -> Property
prop_optional e s = lbl [s] $
    isAvail e t ?== True
    .&.
    case isAvail e s of
        True  -> assertEqualAvailHtml e t s 
        -- FIX
        False -> (binAsrt "Unavailable optional elements must not generate messages." (null $ msgs e t)) .&. True
    where t = S.optional s

prop_exactly :: BibtexEntry -> Field -> Property
prop_exactly e@(Entry _ _ fs) f = property $
    isAvail e (S.exactly f) ?== (isJust $ lookup f fs)

prop_avail_msgs :: BibtexEntry -> SpecTree -> Property
prop_avail_msgs e s = lbl [s] $
    isAvail e s ?/= any isError (msgs e s)
    where isError (Error _) = True
          isError _         = False


-- | The entry point of the test suite
main :: IO ()
main = do 
  r1 <- quickCheckResult prop_either
  r2 <- quickCheckResult prop_both
  r3 <- quickCheckResult prop_optional
  r4 <- quickCheckResult prop_exactly
  r5 <- quickCheckResult prop_avail_msgs
  if passQ [r1, r2, r3, r4, r5]
    then return ()
    else exitFailure

lbl ss c = label ("Tree depth: " ++ (show $ maximum $ map depth ss)) c
