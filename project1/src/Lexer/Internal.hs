-- This module defines basic lexers for the tokens of a bibtex file

module Lexer.Internal where

import CCO.Lexing hiding (satisfy)
import Control.Applicative

-- | The tokens defined for a bibtex file
data Token 
  = AtSign      -- ^ '@'
  | LBracket    -- ^ '{'
  | RBracket    -- ^ '}'
  | Comma       -- ^ ','
  | EqualSign   -- ^ '='
  | Identifier String   -- ^ Field names and entry types
  | Value String        -- ^ The content of a field
  deriving (Show, Eq)

-- | TODO use space
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

-- | A lexer that tokenize an identifier (field name or key)
identifier = Identifier <$> iden
  where iden = (:) <$> alpha <*> many alphaNum

