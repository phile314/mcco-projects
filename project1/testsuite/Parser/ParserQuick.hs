-- | This module tests the parser using quickcheck

module ParserQuick where

import Bibtex (Field, Type)
import CCO.Feedback (runFeedback)
import CCO.Parsing (parse_)
import CCO.SourcePos (Source(Stdin))
import Lexer (lexer)
import Parser (pField, pType, TokenParser)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO (stderr)
import Utility

-- | The tests that will be run
tests :: [Property]
tests = [property testFieldParser, property testTypeParser]

-- | @'testParser' p input expected@ tests that the parser @p@ run on @input@ returns
-- @expected@.
testParser :: Eq a => TokenParser a -> String -> a -> Property
testParser p input expected = monadicIO $ wp m test 
  where m = runFeedback (parse_ lexer p Stdin input) 1 1 stderr
        test (Just actual) = assert (expected == actual)
        test Nothing       = error "Parser Failed"

-- | Tests the field parser 'pField'.
testFieldParser :: Field -> Property
testFieldParser f = testParser pField (show f) f 

-- | Tests the type parser 'pType'
testTypeParser :: Type -> Property
testTypeParser t = testParser pType (show t) t
