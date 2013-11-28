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

-- | Converts a bibtex entry to html. 
entryToHtml :: BibtexEntry -> Feedback [HtmlTree]
entryToHtml e@(Entry t _ _) = messages msgs >> return (getHtml res)
  where res = walkTree fieldToHtml e (spec t)
        msgs = getMsgs res
