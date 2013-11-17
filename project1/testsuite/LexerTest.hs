-- | This module contains lexer tests for test-driven-development

module Main where

import Test.HUnit
import System.Exit (exitFailure)
import CCO.Lexing
import CCO.SourcePos
import Lexer
import Prelude hiding (lex)

-- | The single tests that will be run
tests :: Test
tests = TestList [testSimpleLexers]

testLexer :: (Lexer Token, String, Token) -> Test
testLexer (lexer, input, expected) = TestLabel ("Simple" ++ show expected) $
  expected ~=? actual
    where Symbols _ [Token actual _ _ _] = lex lexer Stdin input

testSimpleLexers :: Test
testSimpleLexers = TestList xs
  where xs = map testLexer [(atSign, "@", AtSign),
                            (lBracket, "{", LBracket)]

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
