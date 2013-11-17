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

-- | A lexer that recognizes '}'
rBracket :: Lexer Token
rBracket = char '}' *> pure RBracket

-- | A lexer that recognizes ','
comma :: Lexer Token
comma = char ',' *> pure Comma

-- | A lexer that recognizes '='
equal :: Lexer Token
equal = char '=' *> pure EqualSign

-- | A lexer that tokenizes the value of a field
value :: Lexer Token
value = Value <$> (inQuotes <|> inBrackets <|> number) -- TODO special latex syntax not handled
  where inQuotes   = char '"' *> some (anyCharBut "\"") <* char '"'
        inBrackets = char '{' *> some (anyCharBut "}") <* char '}'
        number     = some digit
