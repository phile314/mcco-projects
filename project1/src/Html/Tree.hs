{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Data types to represent a HTML tree.
module Html.Tree where

import CCO.Tree
import Control.Monad

-- | An html tree.
type HtmlTree = Node

-- | Represents an html tag
type ElemName = String

-- | Represents the name of an attribute inside an html tag
type AttrName = String

-- | Represents the value of an attribute inside an html tag
type AttrVal = String

-- | A HTML node.
data Node
    = Text String
    | Elem ElemName [Node] [(AttrName, AttrVal)]
    deriving (Show)

instance Tree Node where
    fromTree (Text s)                   = App "Text" [fromTree s]
    fromTree (Elem e ns attrs)          = App "Elem" [fromTree e, fromTree ns, fromTree attrs]

    toTree (App "Text" [s])             = liftM Text (toTree s)
    toTree (App "Elem" [e, ns, attrs])  = liftM3 Elem (toTree e) (toTree ns) (toTree attrs)

