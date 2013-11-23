-- | This module contains the program pp-html.
-- It reads from stdin an 'ATerm' representing an 'HtmlTree' and prints the 
-- correspondent html source code to stdout.

module Main (main) where

import CCO.Component hiding (parser)
import CCO.Feedback (Feedback)
import CCO.Tree (toTree, parser, ATerm)
import Control.Arrow ((>>>))
import Html.Tree (HtmlTree)
import Html.PrettyPrinter

-- | The entry point of the program
main :: IO ()
main = ioWrap (parser >>> component (toTree :: ATerm -> Feedback HtmlTree) >>> printer)
