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
import Options

-- | The entry point of the program
main :: IO ()
main = do
    sane <- useSaneSerialize
    if sane then
        ioWrap (parser >>> arr fromTree >>> arr show)
        else
             ioWrap (parser >>> arr fromTree >>> printer)
