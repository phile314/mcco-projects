{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Data types to represent a HTML tree.
module Html.Tree where

import CCO.Tree
import Control.Monad

-- | A html document.
data HtmlDoc = HtmlDoc HtmlTree

-- | A html tree.
type HtmlTree = Node

-- | Represents a html tag
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



-- | A class for things from which a html representation can be derivied.
class Html a where
  -- | Returns an html representation
  toHtml :: a -> HtmlTree

-- | Converts something into a html document.
toHtmlDoc :: Html a => a -> HtmlDoc
toHtmlDoc = HtmlDoc . toHtml


instance Tree HtmlDoc where
    fromTree (HtmlDoc t)                = App "HtmlDoc" [fromTree t]
    toTree   (App "HtmlDoc" [t])        = liftM HtmlDoc (toTree t)

instance Tree Node where
    fromTree (Text s)                   = App "Text" [fromTree s]
    fromTree (Elem e attrs ns)          = App "Elem" [fromTree e, fromTree attrs, fromTree ns]

    toTree (App "Text" [s])             = liftM Text (toTree s)
    toTree (App "Elem" [e, attrs, ns])  = liftM3 Elem (toTree e) (toTree attrs) (toTree ns)


infixr 5 <<
-- | @name << tree@ wraps the attributless element named @name@ around @tree@.
(<<) :: ElemName  -- ^ The name of the outer element
     -> HtmlTree  -- ^ The html element that gets nested
     -> HtmlTree
name << tree = Elem name [] [tree]
