-- | The attribute grammar used for converting a 'BibtexEntry' to 'Html'.
--  
--   The bibtex field specification is constructed as a tree, and then a tree
--   fold/walk is performed using the given field names/values as input. The output
--   is the generated html and warning/error messages. All error/warning messages
--   are collected even if an error is encountered. Html output is only produced,
--   if there were no errors.
--   
-- * Implementation remarks
--   
--   (The term attribute in this text is referring to the attribute grammar,
--    the term field is used to refer to fields of a bibtex entry.)
--
--   It is assumed that the printing of any field is indepent from all
--   other fields. To print an entry, it is therefore sufficient
--   to sort the fields in the correct order and call `fieldToHtml`
--   for each field. 
--   This is achieved using a tree specifying allowed fields, their 
--   combinations and their order. Then a fold
--   over this tree takes place, carrying the field values as attribute
--   throughout the tree. Each node of the tree then decides how to print a
--   subset of this fields.
--
--   For sorting alone the tree might not be strictly necessary,
--   but Bibtex is more complicated as it sometimes allows
--   a mutually-exclusive choice between two fields and a variety
--   of optional fields. As it is also a good idea to validate that
--   a valid set of field values is given, a simple list sorter
--   does not suffice.
--
-- * Usage remarks
--   
--   The function `entryToHtml` is intended as sole entry point for
--   users of this module.
--   (TODO: do not export all the other definitions. How to do this in uuagc?)

module BibHtml.Converter where

import Html.Tree
import CCO.Feedback
import CCO.Printing
import Bibtex
import Data.Map hiding (map)
import qualified Data.Map as M
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

