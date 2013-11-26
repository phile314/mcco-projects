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
    | Elem ElemName [(AttrName, AttrVal)] [Node]
    deriving (Show, Eq)

instance Tree Node where
    fromTree (Text s)                   = App "Text" [fromTree s]
    fromTree (Elem e attrs ns)          = App "Elem" [fromTree e, fromTree attrs, fromTree ns]

    toTree (App "Text" [s])             = liftM Text (toTree s)
    toTree (App "Elem" [e, attrs, ns])  = liftM3 Elem (toTree e) (toTree attrs) (toTree ns)

