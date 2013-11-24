-- This module defines a lexer for a Bibtex database

module Lexer (
    lexer
  , pIdentifier
  , pValue
  , pToken
  , Token(..)
  , isIdentifier
  , runLexer
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
  describe (Decl _) = ("entry declaration `@entry{' " ++)

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
        lexers = [whitespace, decl, lBracket, rBracket, 
                  comma, equal, value, identifier]

-- | Runs 'lexer' performing some simplification of the tokens.
runLexer :: String -> Symbols Token
runLexer = postProcess . (L.lex lexer Stdin)
  where postProcess (Symbols s xs) = Symbols s (concatMap simplify xs)

-- Convert complex tokens into simple tokens.
-- This is needed because 'CCO.Lexing' does not allow to write contex sensitive
-- lexers (required for bibtex files). Therefore in order to make it contex-free
-- some tokens must be merged, however this make the parsing unnatural.
-- For instance the 'Token' @'Decl s'@ is splitted in three different tokens:
-- @[AtSign, Identifier s, LBracket]@.
simplify :: LexicalUnit Token -> [LexicalUnit Token]
simplify (Token (Decl s) p _ s2) = [at, identifier, bracket]
  where at         = Token AtSign p "@" (s ++ "{")
        identifier = Token (Identifier s) (skip 1 p) s ("{" ++ s2)
        bracket    = Token LBracket (skip (1 + length s) p) "{" s2 
simplify t = [t]

-- | @'skip' n p@ returns @p@ in which @n@ column numbers have been skipped.
skip :: Int -> Pos -> Pos
skip n (Pos l c) = Pos l (c + n)

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
