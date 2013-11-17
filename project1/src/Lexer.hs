-- This module defines a lexer for a Bibtex database

module Lexer where

import CCO.Lexing
import Control.Applicative

-- | The token recognized by the lexer
data Token 
  = AtSign      -- ^ '@'
  | LBracket    -- ^ '{'
  | RBracket    -- ^ '}'
  | Comma       -- ^ ','
  | EqualSign   -- ^ '='
  | Quote       -- ^ '"'
--  | Special String      -- ^ Special latex sintax e.g {G}, {\"o} ...
  | Identifier String   -- ^ Field names and entry types
  | Value String        -- ^ The content of a field
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Lexer
------------------------------------------------------------------------------

-- | A lexer that recognizes and consumes (ignores) whitespace
whitespace :: Lexer Token
whitespace = ignore (some (anyCharFrom " \n\t"))

-- | A lexer that recognizes '@'
atSign :: Lexer Token
atSign = char '@' *> pure AtSign

-- | A lexer that recognizes '{'
lBracket :: Lexer Token
lBracket = char '{' *> pure LBracket
