-- | This module contains lexer tests for test-driven-development

module Main where

import Test.HUnit
import System.Exit (exitFailure)
import CCO.Lexing
import CCO.SourcePos
import Lexer.Internal
import Prelude hiding (lex)

-- | The single tests that will be run
tests :: Test
tests = TestList [testSimpleLexers]

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

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
