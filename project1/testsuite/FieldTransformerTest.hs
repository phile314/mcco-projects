-- | This module tests the 'Bibtex' 'Tree' instance.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Bibtex
import BibHtml.Spec
import BibHtml.Converter
import Data.Maybe (isJust)
import Data.List (lookup) 

import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Utility


prop_either :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_either e s1 s2 = lbl [s1, s2] $
    (isAvail e t) == (isAvail e s1 || isAvail e s2)
    &&
    case (isAvail e s1, isAvail e s2) of
        (True, _)      -> (snd $ e2h t) == (snd $ e2h s1)
        (False, True)  -> (snd $ e2h t) == (snd $ e2h s2)
        (False, False) -> (snd $ e2h t) == Nothing
    where t = Either s1 s2
          e2h = \s -> entryToHtml1 s e

prop_both :: BibtexEntry -> SpecTree -> SpecTree -> Property
prop_both e s1 s2 = lbl [s1, s2] $
    (isAvail e t) == (isAvail e s1 && isAvail e s2)
    &&
    (isAvail e s1 && isAvail e s2) == (isJust $ snd $ entryToHtml1 t e)
    where t = (Both s1 s2)

prop_optional :: BibtexEntry -> SpecTree -> Property
prop_optional e s = lbl [s] $
    isAvail e (Optional s) == True
    &&
    case isAvail e s of
        True  -> (snd $ entryToHtml1 (Optional s) e) == (snd $ entryToHtml1 s e)
        False -> isJust (snd $ entryToHtml1 (Optional s) e)

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
