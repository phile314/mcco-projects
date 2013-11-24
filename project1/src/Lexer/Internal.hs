-- This module defines basic lexers for the tokens of a bibtex file

module Lexer.Internal where

import CCO.Lexing hiding (satisfy)
import Control.Applicative
import Data.Maybe (fromMaybe)

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

-- | A lexer that recognizes and consumes (ignores) whitespace and comments.
-- (Latex) comments start with the char '%' until the eol.
whitespace :: Lexer Token
whitespace = ignore (comment <|> some space)
  where comment = char '%' *> many (anyCharBut "\n\r")

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
value = Value <$> (inQuotes <|> number)
  where inQuotes = concat <$> (char '"' *> some content <* char '"')
        number   = some digit
        content = special <|> ((:[]) <$> anyCharBut "\"") -- FIX not very efficient
        special  = convert <$> (char '{' *> many (anyCharBut "}") <* char '}')
 
-- | A lexer that tokenize an identifier (field name or key)
identifier :: Lexer Token
identifier = Identifier <$> iden
  where iden = (:) <$> alpha <*> many alphaNum

-- | Converts a string that contains latex special syntax
convert :: String -> String
convert s = fromMaybe s $ lookup s conversionRules

-- | An association list that contains the conversion rules from latex special syntax to 
-- utf8 characters.
conversionRules :: [(String, String)]
conversionRules = [("\\`e", "è"), ("\\\"o", "ö")]
