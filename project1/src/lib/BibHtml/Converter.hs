-- | This module contains functions to convert from bibtex elements
-- to html representation.

module BibHtml.Converter where

import Html.Tree
import CCO.Feedback
import CCO.Printing
import Data.List (intersperse)
import Bibtex
import BibHtml.BibtexSpec
import BibHtml.Spec

instance Html BibtexEntry where
  toHtml (Entry t _ xs) = Elem "span" [] $ map (fieldToHtml t) xs

instance Html BibtexDb where
  toHtml (BibtexDb db) = Elem "html" [] [header, body]
    where header = "head" << "title" << Text "Bibliography"
          body = Elem "body" [] (toc ++ [table])
          toc  = summaryOf db
          table = tableOf db

-- | Converts an attribute to a html tree.
fieldToHtml :: Type -> (Field, String) -> HtmlTree
fieldToHtml Inproceedings (Title, s) = Text s
fieldToHtml Inproceedings (Booktitle, s) = Elem "em" [] [Text s]
fieldToHtml _ (Title, s) = Elem "em" [] [Text s]
fieldToHtml _ (Editor, s) = Text $ "In: " ++ s
fieldToHtml _ (f, s) = Text s


-- | Produces a summary of the given bibtex entries.
-- More precisely it is an html object containing a list of 
-- anchors referring to the associated entry. 
summaryOf :: [BibtexEntry] -> [HtmlTree]
summaryOf db = intersperse (Text " | ") anchors
  where anchors = map anchor db

-- | Produces an html two columns table containing the given bibtex entries.
-- The first column contains the 'deriveKey's, whereas the second contains
-- the given html description of the correspondent bibtex entry.
tableOf :: [BibtexEntry] -> HtmlTree
tableOf db = Elem "table" [("border", "0")] $ map (row . process) db
  where row htmls = Elem "tr" [("valign", "top")] $ map ("td" <<) htmls
        process e = [reference e, toHtml e]

-- | Produces a reference html object, whose name is the key of the entry.
-- The name displayed will be the result of 'deriveKey' on the key entry.
reference :: BibtexEntry -> HtmlTree
reference e@(Entry _ k _) = Elem "a" [("name", k)] [Text key]
  where key = "[" ++ (deriveKey e) ++ "]"

-- | Produces an anchor html object that points to the entry key.
-- The visible name is the result of 'deriveKey' on the key entry.
anchor :: BibtexEntry -> HtmlTree
anchor e@(Entry _ k _) = Elem "a" [("href", '#':k)] [Text key]
  where key = "[" ++ (deriveKey e) ++ "]"

-- | Derives the key shown to the user when referring to an entry, eg "[LO98]".
--TODO derive key correctly
deriveKey :: BibtexEntry -> String
deriveKey (Entry t k fs) = k
