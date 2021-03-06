-- | This is the entry point of the program bib2html.
-- It reads from stdin an ATerm representing a bibtex database and
-- converts it to Html, whose ATerm is printed on the stdout.
-- This programs checks whether all the required fields are present,
-- and if this is the case an error is raised.
-- If some uknown (non optional field) is used, a warning is raised
-- and the field is ignored.

module Main (main) where

import Bibtex
import BibHtml.BibtexSpec
import BibHtml.Converter
import BibHtml.Validator (validate)
import CCO.Component (ioWrap, component, printer)
import CCO.Feedback (Feedback, messages)
import CCO.Tree (parser, toTree, fromTree, ATerm)
import Control.Arrow ((>>>), arr)
import Data.List (sortBy)
import Html.Tree (Node, HtmlTree, toHtmlDoc)
import Options

-- | The entry point of the program
main :: IO ()
main = do
    sane <- useSaneSerialize
    if sane then
        ioWrap $ arr (read :: String -> ATerm) >>> component toTree >>> arr sorter >>> component validate >>> arr toHtmlDoc >>> arr fromTree >>> arr show
        else
            ioWrap $ parser >>> component toTree >>> arr sorter >>> component validate >>> arr toHtmlDoc >>> arr fromTree >>> printer

-- | Return the given 'BibtexDb' sorted first by author and then by year and title.
-- If the considered field is missing in some entry, such entry will come after 
-- those that have that field.
sorter :: BibtexDb -> BibtexDb
sorter (BibtexDb d) = BibtexDb $ sortBy criteria d
  where criteria (Entry _ _ xs) (Entry _ _ ys) = compare (ayt xs) (ayt ys)
        ayt xs = (lookup Author xs, lookup Year xs, lookup Title xs)
