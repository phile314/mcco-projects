-- | This module contains functions to convert from bibtex elements
-- to html representation.

module BibHtml.Converter where

import Html.Tree
import CCO.Feedback
import CCO.Printing
import Bibtex
import BibHtml.BibtexSpec
import BibHtml.Spec


-- | Converts an attribute to a html tree.
fieldToHtml :: Type -> (Field, String) -> HtmlTree
fieldToHtml Inproceedings (Title, s) = Text s
fieldToHtml Inproceedings (Booktitle, s) = Elem "em" [] [Text s]
fieldToHtml _ (Title, s) = Elem "em" [] [Text s]
fieldToHtml _ (Editor, s) = Text $ "In: " ++ s
fieldToHtml _ (f, s) = Text s


-- | Derives the key shown to the user when referring to an entry, eg "[LO98]".
deriveDispKey :: BibtexEntry -> String
--TODO derive key correctly
deriveDispKey (Entry t k fs) = k


-- | Converts a bibtex entry to html. If the first element of the result contains
--   no error messages, the second element is a html tree. If there are error
--   messages, `Nothing` will be returned as second element.
entryToHtml :: BibtexEntry -> ([Message], Maybe (HtmlTree, HtmlTree))
entryToHtml e@(Entry t _ _) = entryToHtml1 (spec t) e

entryToHtml1 :: SpecTree -> BibtexEntry -> ([Message], Maybe (HtmlTree, HtmlTree))
entryToHtml1 s e@(Entry _ k _) = if avail_Syn_SpecTree res then (msgs, Just (htmlInd, htmlEnt)) else (msgs, Nothing)
    where
        res = walkTree fieldToHtml e s
        msgs = msgs_Syn_SpecTree res

        isError (Error _) = True
        isError _ = False

        htmlInd = Elem "a" [("href", k)] [dKey]
        htmlEnt = Elem "tr" [("valign", "top")] [
                            Elem "td" [] [Elem "a" [("name", k)] [dKey]],
                            Elem "td" [] (html_Syn_SpecTree res)]
        dKey = Text $ "[" ++ (deriveDispKey e) ++ "]"

