-- | This module defines some utility feature needed for testing purposes

module Utility where

import Bibtex
import Control.Monad (liftM2, liftM3)
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

instance Arbitrary BibtexEntry where
  -- TODO produced fields should be valid for the chosen type
  arbitrary = liftM3 Entry types arbitrary (listOf1 fieldValues)
    where fieldValues = liftM2 (,) fields values

-- | A generator of Bibtex entry types
types :: Gen String
types = elements ["article", "book", "booklet", "conference", "inbook",
                  "incollection", "inproceedings", "manual", "masterthesis",
                  "misc", "phdthesis", "techreport", "unpublished"]

-- | A generator of Bibtex fields
fields :: Gen String 
fields = elements [ "address", "annote", "author", "booktitle", "chapter",
                    "crossref", "edition", "editor", "howpublished", 
                    "institution", "journal", "key", "month", "note", "number",
                    "organization", "pages", "publisher", "school", "series",
                    "title", "type", "volume", "year"]

-- | A generator for values associated with a field
values :: Gen String
values = listOf1 $ suchThat arbitrary (/= '"')

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
bibtexDbATerm = listOf (arbitrary :: Gen BibtexEntry) >>= return . fromTree

-- | A comment is a string line starting with '%'
newtype Comment = C String 
  deriving (Show, Eq)

instance Arbitrary Comment where
  arbitrary = listOf (suchThat arbitrary noLineFeed) >>= return . C . ('%':)
    where noLineFeed c = c /= '\n' && c /= '\r'
