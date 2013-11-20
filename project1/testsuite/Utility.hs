-- | This module defines some utility feature needed for testing purposes

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Utility where

import Bibtex
import Control.Monad (liftM3, liftM)
import CCO.Lexing (LexicalUnit(..), Symbols(..))
import CCO.Tree (ATerm, fromTree)
import Lexer
import Test.QuickCheck

instance Show a => Show (LexicalUnit a) where
  show (Token a p s1 s2) = unwords [show a, show p, s1, s2]
  show (Error p s1 s2)   = unwords ["Error", show p, s1, s2]
  show (Msg m p s1 s2)   = unwords ["Msg", m, show p, s1, s2]

instance Show a => Show (Symbols a) where
  show (Symbols s xs) = unwords ["Symbols", show s, show xs]

instance Arbitrary BibtexDb where
  arbitrary = liftM BibtexDb arbitrary

instance Arbitrary BibtexEntry where
  arbitrary = liftM3 Entry arbitrary arbitrary (listOf1 arbitrary)

instance Arbitrary Key where
  arbitrary = identifier 

{-
instance Arbitrary Field where
  arbitrary = identifier    -- TODO a more specific generator should be used

instance Arbitrary Type where
  arbitrary = identifier    -- TODO a more specific generator should be used

instance Arbitrary Value where
  arbitrary = listOf1 . (suchThat arbitrary (/= "\""))
-}

-- | Generator for identifier ([a-z][a-zA-Z0-9]*)
identifier :: Gen String
identifier = do
    c1 <- choose ('a','z')
    cn <- listOf . elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return (c1:cn)

-- | A generator of ATerm representing 'BibtexEntry' items.
bibtexEntryATerm :: Gen ATerm
bibtexEntryATerm = (arbitrary :: Gen BibtexEntry) >>= return . fromTree

-- | A generator of ATerm representing 'BibtexDb'.
bibtexDbATerm :: Gen ATerm
bibtexDbATerm = (arbitrary :: Gen BibtexDb) >>= return . fromTree
