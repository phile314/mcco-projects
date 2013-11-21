-- | This is the entry point of the program bib2html.
-- It reads from stdin an ATerm representing a bibtex database and
-- converts it to Html, whose ATerm is printed on the stdout.
-- This programs checks whether all the required fields are present,
-- and if this is the case an error is raised.
-- If some uknown (non optional field) is used, a warning is raised
-- and the field is ignored.

module Main (main) where

import Bibtex
import CCO.Component (ioWrap, component, printer)
import CCO.Feedback (Feedback)
import CCO.Tree (parser, toTree, fromTree, ATerm)
import Control.Arrow ((>>>), arr)
import HtmlTree (Node)

-- | The entry point of the program
main :: IO ()
main = ioWrap $
  parser >>> component toTree >>> arr sorter >>> component toHtml >>> arr fromTree >>> printer

-- | Return the given 'BibtexDb' sorted first by author and then by year and title.
sorter :: BibtexDb -> BibtexDb
sorter = undefined

-- | Converts a 'BibtexDb' in an html 'Node.
toHtml :: BibtexDb -> Feedback Node
toHtml = undefined
