-- | This module defines functions to convert a 'BibtexDb' to
-- an html representation.
-- TODO should be merged with Converter.

module BibHtml.DbTransformer
    (toHtml)
where

import Bibtex
import CCO.Feedback
import Data.List (intersperse)
import Html.Tree
import Data.Maybe
import BibHtml.Converter

-- | Returns an html representation of a 'BibtexDb'.
-- Note that the result is wrapped in the 'Feedback' monad because this 
-- operation may fail, for instance when some required field of a bibtex 
-- entry are missing.
toHtml :: BibtexDb -> Feedback HtmlTree
toHtml (BibtexDb db) = do
    table <- mapM entryToHtml db >>= return . (tableOf db)
    let body = Elem "body" [] (toc ++ [table])
    return $ Elem "html" [] [header, body]
  where header = "head" << "title" << Text "Bibliography"
        toc    = makeSummary db

-- | Produces a summary of the given bibtex entries.
-- More precisely it is an html object containing a list of 
-- anchors referring to the associated entry. 
makeSummary :: [BibtexEntry] -> [HtmlTree]
makeSummary db = intersperse (Text " | ") anchors
  where anchors = map reference db

-- | Produces an anchor html object that points to the entry key.
-- The visible name is the result of 'deriveKey' on the key entry.
reference :: BibtexEntry -> HtmlTree
reference e@(Entry _ k _) = Elem "a" [("href", '#':k)] [Text key]
  where key = "[" ++ (deriveKey e) ++ "]"

-- | Derives the key shown to the user when referring to an entry, eg "[LO98]".
--TODO derive key correctly
deriveKey :: BibtexEntry -> String
deriveKey (Entry t k fs) = k

-- | Produces a two column table, in which each row correspond to 
-- each entry of the given list of 'BibtexEntry'.
-- The first column contains the 'deriveKey's, whereas the second contains
-- the given html description of the correspondent bibtex entry.
tableOf :: [BibtexEntry] -> [[HtmlTree]] -> HtmlTree
tableOf db ht = Elem "table" [("border", "0")] $ map process (zip db ht)
  where row xs = Elem "tr" [("valign", "top")] $ map ("td" <<) xs
        process (e,h) = row $ (reference e):h
