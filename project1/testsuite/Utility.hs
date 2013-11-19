-- | This module defines some utility feature needed for testing purposes

module Utility where

import CCO.Lexing (LexicalUnit(..), Symbols(..))
import Lexer

instance Show a => Show (LexicalUnit a) where
  show (Token a p s1 s2) = unwords [show a, show p, s1, s2]
  show (Error p s1 s2)   = unwords ["Error", show p, s1, s2]
  show (Msg m p s1 s2)   = unwords ["Msg", m, show p, s1, s2]

instance Show a => Show (Symbols a) where
  show (Symbols s xs) = unwords ["Symbols", show s, show xs]
