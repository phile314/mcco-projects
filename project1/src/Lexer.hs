-- This module defines a lexer for a Bibtex database

module Lexer (
    lexer
  , pIdentifier
  , pValue
  , pToken
  , Token(..)
  ) where

import CCO.Lexing hiding (satisfy)
import CCO.Parsing
import Control.Applicative
import Data.List
import Lexer.Internal 
import Lexer.Internal (Token(..))

instance Symbol Token where
  describe AtSign = ("@ " ++)
  describe LBracket = ("{ " ++)
  describe RBracket = ("} " ++)
  describe Comma = (", " ++)
  describe EqualSign = ("= " ++)
  describe (Identifier _) = ("identifier " ++)
  describe (Value _) = ("value " ++)

-- | Tests whether a token is an identifier
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier  _             = False

-- | Tests whether a token is a value
isValue :: Token -> Bool
isValue (Value _) = True
isValue  _        = False

------------------------------------------------------------------------------
-- Lexer
------------------------------------------------------------------------------

-- | A lexer for a bibtex file
lexer :: Lexer Token
lexer = choice lexers 
  where choice = foldr1 (<|>)
        lexers = [whitespace, atSign, lBracket, rBracket, 
                  comma, equal, value, identifier]

-------------------------------------------------------------------------------
-- Utility Parser
-------------------------------------------------------------------------------

-- | Parses a given token and returns it
pToken :: Token -> Parser Token Token
pToken t = satisfy (t ==) <!> describe t ""

-- | Parses a value token and returns the string content
pValue :: Parser Token String
pValue = (\(Value s) -> s) <$> satisfy isValue 

-- | Parses an identifier token and returns the string content
pIdentifier :: Parser Token String
pIdentifier = (\(Identifier s) -> s) <$> satisfy isIdentifier  
                                     <!> describe (Identifier "") ""
