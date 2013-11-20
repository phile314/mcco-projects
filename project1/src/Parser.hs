-- | This module defines a parser for a bibtex file

module Parser where

import Bibtex
import CCO.Component
import qualified CCO.Component as C (parser)
import CCO.Parsing
import Control.Applicative
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
pEntry = Entry <$> (pAt *> pIdentifier) <*> pKey <*> pData
  where pKey = pLBracket *> pIdentifier <* pComma
        pData = (manySepBy pComma pFieldValue) <* pRBracket
        pFieldValue = (,) <$> (pIdentifier <* pEq) <*> pValue

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
