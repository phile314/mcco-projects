-- | This is the entry point of the program parse-bib.
-- It reads from stdin a bibtex file and prints on the stdout 
-- an 'ATerm' that describes the structure of the database.

module Main (main) where

import Control.Arrow
import CCO.Component (ioWrap, printer)
import CCO.Printing (pp, render_)
import CCO.Tree (fromTree)
import Parser (parser)
import Lexer

-- | The entry point of the program
main :: IO ()
main = ioWrap (parser >>> arr fromTree >>> printer)
