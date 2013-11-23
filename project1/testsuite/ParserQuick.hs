-- | This module tests the parser using quickcheck

module ParserQuick where

import Bibtex (Field)
import CCO.Feedback (runFeedback)
import CCO.Parsing (parse_)
import CCO.SourcePos (Source(Stdin))
import Lexer (lexer)
import Parser (pField)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO (stderr)
import Utility

-- | The tests that will be run
tests :: [Property]
tests = [property testFieldParser]

-- | Tests the field parser 'pField'.
testFieldParser :: Field -> Property
testFieldParser expected = monadicIO $ wp m test 
  where m = runFeedback (parse_ lexer pField Stdin (show expected)) 1 1 stderr
        test (Just actual) = assert (expected == actual)
        test Nothing       = error "Parser Failed"
