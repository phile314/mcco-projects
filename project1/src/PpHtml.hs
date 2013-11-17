-- | Produces a html string from a html tree representation.
module Main
    ( main
    , flattenHtmlTree )
where

import HtmlTree
import CCO.Component hiding (parser)
import CCO.Printing
import Control.Arrow
import CCO.Tree

-- | Indentation spacing.
ind = 4

main = ioWrap (parser >>> component toTree >>> flattenHtmlTree)

-- | Converts a `Node` to a string. No validation of the tree
--   structure takes place. Special characters are escaped properly
--   by this function (TODO).
flattenHtmlTree :: Component Node String
flattenHtmlTree = component (\n -> do
    let doc = toDoc n
    return $ render_ 200 doc)


--TODO escaping

-- | Converts a `Node` to a `Doc`.
toDoc (Text s) = text s
toDoc (Elem t [] attrs) = do
    enclose langle (text "/" >|< rangle) (text t >|< toDocA attrs)
toDoc (Elem t es attrs) = do
    angles (text t >|< toDocA attrs)
        >-< (indent ind $ toDocE es)
        >-< (enclose (langle >|< text "/") rangle (text t))


--TODO escaping

toDocA :: [(AttrName, AttrVal)] -> Doc
toDocA [] = empty
toDocA ((an, av):as) = space >|< text an >|< text "=" >|< (quoted $ text av) >|< toDocA as

quoted d = enclose q q d
    where q = text "\""

toDocE :: [Node] -> Doc
toDocE es = foldl (\a c -> a >|< (toDoc c)) empty es
