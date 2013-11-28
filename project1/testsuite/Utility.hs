-- | This module defines some utility feature needed for testing purposes

module Utility where

import qualified BibHtml.Spec as S
import Bibtex
import Control.Monad (liftM, liftM2, liftM3)
import CCO.Lexing (LexicalUnit(..), Symbols(..))
import CCO.Tree (ATerm, fromTree)
import Lexer
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Test

instance Show a => Show (LexicalUnit a) where
  show (Token a p s1 s2) = unwords [show a, show p, s1, s2]
  show (Error p s1 s2)   = unwords ["Error", show p, s1, s2]
  show (Msg m p s1 s2)   = unwords ["Msg", m, show p, s1, s2]

instance Show a => Show (Symbols a) where
  show (Symbols s xs) = unwords ["Symbols", show s, show xs]

instance Arbitrary BibtexDb where
  arbitrary = liftM BibtexDb arbitrary

instance Arbitrary BibtexEntry where
  -- TODO produced fields should be valid for the chosen type
  arbitrary = liftM3 Entry arbitrary arbitrary (listOf1 fieldValues)
    where fieldValues = liftM2 (,) arbitrary values

-- | A generator of Bibtex entry types
instance Arbitrary Type where
  arbitrary = elements types

-- | A generator of Bibtex fields
instance Arbitrary Field where
    arbitrary = elements fields

-- | A generator of spec trees.
instance Arbitrary S.SpecTree where
    arbitrary = sized tree
        where
            tree 0 = liftM S.exactly arbitrary
            tree n | n > 0 = let sub = tree (n `div` 2) in
                              oneof [  liftM S.exactly arbitrary
                                    , liftM2 S.both sub sub
                                    , liftM2 S.either sub sub
                                    , liftM S.optional sub]

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
bibtexDbATerm = (arbitrary :: Gen BibtexDb) >>= return . fromTree

-- | A comment is a string line starting with '%'
newtype Comment = C String 
  deriving (Show, Eq)

instance Arbitrary Comment where
  arbitrary = listOf (suchThat arbitrary noLineFeed) >>= return . C . ('%':)
    where noLineFeed c = c /= '\n' && c /= '\r'

-- | Returns whether some 'HUnit' test failed
passH :: Counts -> Bool
passH result = failures result == 0 && errors result == 0

-- | Returns whether some 'QuickCheck' test failed
passQ :: [Result] -> Bool
passQ = all isSuccess

