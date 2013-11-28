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
import BibHtml.Converter (toHtml)
import BibHtml.Spec
import CCO.Component (ioWrap, component, printer)
import CCO.Feedback (Feedback, messages)
import CCO.Tree (parser, toTree, fromTree, ATerm)
import Control.Arrow ((>>>), arr)
import Data.List (sortBy)
import Html.Tree (Node, HtmlTree)
import Options

-- | The entry point of the program
main :: IO ()
main = do
    sane <- useSaneSerialize
    if sane then
        ioWrap $ arr (read :: String -> ATerm) >>> component toTree >>> arr sorter >>> component validateDb >>> arr toHtml >>> arr fromTree >>> arr show
        else
            ioWrap $ parser >>> component toTree >>> arr sorter >>> component validateDb >>> arr toHtml >>> arr fromTree >>> printer

-- | Return the given 'BibtexDb' sorted first by author and then by year and title.
-- If the considered field is missing in some entry, such entry will come after 
-- those that have that field.
sorter :: BibtexDb -> BibtexDb
sorter (BibtexDb d) = BibtexDb $ sortBy criteria d
  where criteria (Entry _ _ xs) (Entry _ _ ys) = compare (ayt xs) (ayt ys)
        ayt xs = (lookup Author xs, lookup Year xs, lookup Title xs)

-- | Validates a 'BibtexDb'.
validateDb :: BibtexDb -> Feedback BibtexDb
validateDb (BibtexDb db) = mapM validate db >>= return . BibtexDb


-- TODO this should be moved to the ag file (cannot due to cycle dependency
-- between spec and Spec.ag
-- | Validates a 'BibtexEntry'.
-- If the validation succeeds the given bitex entry is returned, with
-- the fields ordered according to the specification for its file.
-- Unknown fields or conflicting fields will be removed.
validate :: BibtexEntry -> Feedback BibtexEntry
validate e@(Entry t k _) = messages msgs >> return (Entry t k fs)
  where fs = getBib res
        res = walkTree e (spec t)
        msgs = getMsgs res

