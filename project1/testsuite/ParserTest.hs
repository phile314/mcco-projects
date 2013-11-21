-- | This module contains parser tests for test-driven-development

module Main where

import Bibtex
import CCO.Feedback (runFeedback)
import CCO.Lexing (LexicalUnit(..), Symbols(..))
import qualified CCO.Lexing as C (lex)
import CCO.Parsing (parse)
import CCO.SourcePos
import Lexer
import Parser
import Test.HUnit
import System.Exit (exitFailure)
import System.IO (stderr)
import Utility

-- | @'simpleParseTest' xs p r@ run the parser @p@ on the 
-- input token list @xs@, the tests fails if the result is different from @r@
-- or some token has been not consumed.
simpleParseTest :: (Eq b, Show b) 
                => Symbols Token    -- ^ Input Tokens
                -> TokenParser b    -- ^ The parser tested that will be used
                -> b                -- ^ Expected Result
                -> Test
simpleParseTest input p expected = TestCase $ do
  r <- runFeedback test 1 1 stderr
  case r of
    Just eq -> assertBool "" eq
    Nothing -> assertFailure "Parser failed"
  where test = do
        (actual, Symbols _ xs) <- parse p input
        if null xs 
            then return $ expected == actual
            else error $ "The tokens " ++ show xs ++ " have not been consumed"
    
-- | Tests the entry parser 'pEntry'.
testEntryParser:: Test
testEntryParser = TestLabel "DataParser" $ simpleParseTest input pEntry expected
  where input = C.lex lexer Stdin "@article{key,author=\"bar\"}"
        expected = Entry Article "key" [(Author, "bar")]

-- | Tests the value parser 'pValue'.
testValueParser :: Test
testValueParser = TestLabel "ValueParser" $ simpleParseTest input pValue expected
  where input = C.lex lexer Stdin "\"foobar\""
        expected = "foobar"

-- | The tests that will be run
tests :: Test
tests = TestList [testValueParser, testEntryParser]

-- | The entry point of the test suite
main :: IO ()
main = do  
  result <- runTestTT tests
  if (failures result /= 0 || errors result /= 0)  
    then exitFailure
    else return ()
