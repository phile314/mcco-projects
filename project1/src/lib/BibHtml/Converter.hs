-- | This module contains functions to convert from bibtex elements
-- to html representation.

module BibHtml.Converter where

import Html.Tree
import CCO.Feedback
import CCO.Printing
import Bibtex
import BibHtml.BibtexSpec
import BibHtml.Spec
import Data.List (isPrefixOf)

-- | Converts an attribute to a html tree.
fieldToHtml :: Type -> (Field, String) -> HtmlTree
fieldToHtml Inproceedings (Title, s) = Text s
fieldToHtml Inproceedings (Booktitle, s) = Elem "em" [] [Text s]
fieldToHtml _ (Title, s) = Elem "em" [] [Text s]
fieldToHtml _ (Editor, s) = Text $ "In: " ++ s
-- REMARK: The cco library has a bug when parsing escaped unicode characters, so this will not appear correctly in the html (but it is not our fault....)
fieldToHtml _ (Pages, s) = Text $ replace "--" "—" s
fieldToHtml _ (f, s) = Text s

-- | Converts a bibtex entry to html. 
entryToHtml :: BibtexEntry -> Feedback [HtmlTree]
entryToHtml e@(Entry t _ _) = messages msgs >> return (getHtml res)
  where res = walkTree fieldToHtml e (spec t)
        msgs = getMsgs res


-- There seems to be no str-replace function in the prelude.
-- There is one in the MissingH library, but this library
-- leads to dependency errors when trying to install on my computer (Philipp).

-- | Given a search list and a replacement list, replaces all occurences
--   of the search list with the replacement list in a third list.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace s r i | s `isPrefixOf` i = r ++ (replace s r (drop (length s) i))
replace s r (_:is) | otherwise   = replace s r is
replace _ _ [] = []
