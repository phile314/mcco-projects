-- | This is the entry point of the program parse-bib.
-- It reads from stdin a bibtex file and prints on the stdout 
-- an 'ATerm' that describes the structure of the database.

module Main (main) where

import Control.Arrow ((>>>))
import CCO.Component (ioWrap)
import Parser (parser)

main :: IO ()
main = undefined 
  -- ioWrap (parser >>> show)
