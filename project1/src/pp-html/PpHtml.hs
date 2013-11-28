-- | This module contains the program pp-html.
-- It reads from stdin an 'ATerm' representing an 'HtmlTree' and prints the 
-- correspondent html source code to stdout.

module Main (main) where

import CCO.Component hiding (parser)
import CCO.Feedback (Feedback)
import CCO.Tree (toTree, parser, ATerm)
import Control.Arrow ((>>>), arr)
import Html.Tree (HtmlDoc)
import Html.PrettyPrinter
import CCO.Tree (ATerm)
import System.IO
import Options

-- | The entry point of the program
main :: IO ()
main = do
    hPutStrLn stderr "Please note that the output is in utf8. Some browsers have difficulty detecting the encoding if a file is opened using the \"file://\" scheme, see eg https://bugzilla.mozilla.org/show_bug.cgi?id=760050."
    
    sane <- useSaneSerialize
    if sane then    
        ioWrap (arr (read :: String -> ATerm) >>> component (toTree :: ATerm -> Feedback HtmlDoc) >>> printer)
        else
            ioWrap (parser >>> component (toTree :: ATerm -> Feedback HtmlDoc) >>> printer)
