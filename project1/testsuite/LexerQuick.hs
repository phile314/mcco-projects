-- | In this module the 'Lexer' is further tested using quickcheck

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module LexerQuick where

import CCO.Lexing
import CCO.SourcePos
import Lexer
import Prelude hiding (lex)
import Test.QuickCheck
import Utility

-- | The tests that will be run
tests :: [Property]
tests = [property prop_commentsAreSkipped]

-- | Tests that comments are properly consumed and ignored by the lexer
prop_commentsAreSkipped :: Comment -> Bool
prop_commentsAreSkipped (C comment) = null lexed
  where Symbols _ lexed = lex lexer Stdin comment
