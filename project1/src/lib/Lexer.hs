-- This module defines a lexer for a Bibtex database

module Lexer (
    lexer
  , Token(..)
  ) where

import CCO.Lexing hiding (satisfy)
import qualified CCO.Lexing as L (lex)
import CCO.Parsing
import CCO.SourcePos (Source(Stdin), Pos(..))
import Control.Applicative
import Data.List
import Lexer.Internal 
import Lexer.Internal (Token(..))

instance Symbol Token where
  describe AtSign = ("at sign " ++)
  describe LBracket = ("open bracket " ++)
  describe RBracket = ("closed bracket " ++)
  describe Comma = ("comma " ++)
  describe EqualSign = ("equal sign" ++)
  describe (Identifier _) = ("identifier " ++)
  describe (Value _) = ("value " ++)


------------------------------------------------------------------------------
-- Lexer
------------------------------------------------------------------------------

-- | A lexer for a bibtex file
lexer :: Lexer Token
lexer = choice lexers 
  where choice = foldr1 (<|>)
        lexers = [whitespace, atSign, lBracket, rBracket, 
                  comma, equal, value, identifier]

