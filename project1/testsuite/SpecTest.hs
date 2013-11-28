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


html :: BibtexEntry -> SpecTree -> [HtmlTree]
html e s = html_Syn_SpecTree $ walkTree fieldToHtml e s

msgs :: BibtexEntry -> SpecTree -> [Message]
msgs e s = msgs_Syn_SpecTree $ walkTree fieldToHtml e s


-- | Asserts that either both spec are not available or both return the same tree for a given bitex entry.
assertEqualAvailHtml :: BibtexEntry -> SpecTree -> SpecTree -> Property
assertEqualAvailHtml entry act exp =
    avail_Syn_SpecTree rAct ?== avail_Syn_SpecTree rExp
    .&.
    (avail_Syn_SpecTree rExp ==> html_Syn_SpecTree rAct ?== html_Syn_SpecTree rExp)
    where rAct = walkTree fieldToHtml entry act
          rExp = walkTree fieldToHtml entry exp


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
    (isAvail e t ==> (html e t) ?== ((html e s1) ++ (html e s2)))
    where t = S.both s1 s2

prop_optional :: BibtexEntry -> SpecTree -> Property
prop_optional e s = lbl [s] $
    isAvail e t ?== True
    .&.
    case isAvail e s of
        True  -> assertEqualAvailHtml e t s
        False -> (binAsrt "Unavailable optional elements must not generate messages." (null $ msgs e t)) .&. (html e t ?== [])
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

depth (Both a b)    = 1 + (max (depth a) (depth b))
depth (Either a b _)  = 1 + (max (depth a) (depth b))
depth (Optional a)  = 1 + (depth a)
depth (Exactly _)   = 1

lbl ss c = label ("Tree depth: " ++ (show $ maximum $ map depth ss)) c
