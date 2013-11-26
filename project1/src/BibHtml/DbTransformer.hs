module BibHtml.DbTransformer
    (toHtml)
where

import Bibtex
import CCO.Feedback
import Html.Tree
import Data.Maybe
import BibHtml.Converter

el :: String -> [Node] -> Node
el n c = Elem n [] c

toHtml :: BibtexDb -> Feedback HtmlTree
toHtml (BibtexDb es) = do
    entries <- toHtml1 es
    
    let doc = el "html" [
                el "head" [
                    el "title" [Text "Bibliography"]
                ],
                el "body" ((map fst entries) ++ [
                    el "hr" [],
                    Elem "table" [("border", "0")] (map snd entries)
                ])
            ]
    return doc

wrapE :: HtmlTree -> HtmlTree
wrapE t = Elem "tr" [("valign", "top")] [t]

toHtml1 :: [BibtexEntry] -> Feedback [(HtmlTree, HtmlTree)]
toHtml1 es = do
    let (msgs, htmls) = unzip $ map entryToHtml es

    -- if there is any error, this aborts the conversion.
    messages (concat msgs)
    
    return $ map fromJust htmls
    
