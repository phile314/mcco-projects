-- | This module defines a parser for a bibtex file

module Parser where

import Bibtex
import CCO.Component
import qualified CCO.Component as C (parser)
import CCO.Parsing
import Control.Applicative
import Data.CaseInsensitive (mk)
import Lexer

-- | A parser for streams of tokens
type TokenParser = Parser Token

-- | A 'Component' for Bibtex database files (consumes all the input until eof)
parser :: Component String BibtexDb
parser = C.parser lexer (pDb <* eof)

-- | Parses a bibtex database composed by zero or more entries
pDb :: TokenParser BibtexDb
pDb = BibtexDb <$> many pEntry

-- | Parses a bibtex entry
pEntry :: TokenParser BibtexEntry
pEntry = Entry <$> (pAt *> pType) <*> pKey <*> pData
  where pKey = pLBracket *> pIdentifier <* pComma
        pData = (manySepBy pComma pFieldValue) <* pRBracket
        pFieldValue = (,) <$> (pField <* pEq) <*> pValue

-- | Parses a field 
pField :: TokenParser Field
pField = choice $ map pConstructor fields

-- | Parses an entry Type
pType :: TokenParser Type
pType = choice $ map pConstructor types

-- | Parses an identifier, whose showable constructor is given.
pConstructor :: Show a => a -> TokenParser a
pConstructor c = satisfy (\t -> isIdentifier t && match t) *> pure c
  where match (Identifier s) = mk s == (mk . show) c

-------------------------------------------------------------------------------
-- Basic Parsers
-------------------------------------------------------------------------------
pLBracket :: TokenParser Token
pLBracket = pToken LBracket

pRBracket :: TokenParser Token
pRBracket = pToken RBracket

pComma :: TokenParser Token
pComma = pToken Comma

pAt :: TokenParser Token
pAt = pToken AtSign

pEq :: TokenParser Token
pEq = pToken EqualSign
-- This module defines a lexer for a Bibtex database



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

-- | Tests whether a token is an identifier
isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier  _             = False

-- | Tests whether a token is a value
isValue :: Token -> Bool
isValue (Value _) = True
isValue  _        = False
