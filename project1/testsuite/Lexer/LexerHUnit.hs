-- | This module contains lexer tests for test-driven-development

module LexerHUnit where

import Test.HUnit
import CCO.Lexing
import CCO.SourcePos
import Lexer.Internal
import Prelude hiding (lex)

-- | The single tests that will be run
tests :: Test
tests = TestList [testSimpleLexers, testConversion, testSpecialSyntax]

-- | @'testLexer' (lexer, input, token)@ tests that @token@ 
-- is the single token obtained lexing @input@ using @lexer@
testLexer :: (Lexer Token, String, Token) -> Test
testLexer (lexer, input, expected) = TestLabel ("Simple" ++ show expected) $
  let Symbols _ r = lex lexer Stdin input in
    case r of
    [Token actual _ _ _] -> expected ~=? actual
    _                    -> error "lexer failure"

testSimpleLexers :: Test
testSimpleLexers = TestList $ map testLexer simple
  where simple = [(atSign, "@", AtSign), (lBracket, "{", LBracket), 
                  (rBracket, "}", RBracket), (comma, ",", Comma),
                  (equal, "=", EqualSign), (value, "\"foo\"", Value "foo"),
                  (value, "123", Value "123"),
                  (identifier, "f00b4r", Identifier "f00b4r")]

-- | Tests the 'convert' function.
testConversion :: Test
testConversion = TestLabel "Conversion" $ convert "\\`e" ~?= "è"

-- | Tests the 'value' lexer with special syntax
testSpecialSyntax = TestLabel "SpecialSyntax" $ TestList $ map testLexer ss 
  where ss = [(value, quotes o, Value "ö"),
              (value, quotes (e ++ "foo" ++ e), Value "èfooè")]
        (e, o) = ("{\\`e}", "{\\\"o}")
        quotes s = "\"" ++ s ++ "\"" 
