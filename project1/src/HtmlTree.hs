{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Data types to represent a HTML tree.
module HtmlTree where

import CCO.Tree


type ElemName = String

type AttrName = String
type AttrVal = String

-- | A HTML node.
data Node
    = Text String
    | Elem ElemName [Node] [(AttrName, AttrVal)]

instance Tree Node where
    fromTree (Text s) = App "Text" [fromTree s]
    fromTree (Elem e ns attrs) = App "Elem" [fromTree e, fromTree ns, fromTree attrs]
    toTree (App "Text" [s]) = toTree s >>= return . Text
    toTree (App "Elem" [e, ns, attrs]) = do
        e' <- toTree e
        ns' <- toTree ns
        attrs' <- toTree attrs
        return $ Elem e' ns' attrs'

