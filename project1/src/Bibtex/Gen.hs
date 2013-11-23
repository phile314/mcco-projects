-- TODO should probably be merged with Basic.hs again
-- | The attribute grammar used for converting a 'BibtexDb' in 'Html'.
module Bibtex.Gen where

import CCO.Tree (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser (parseTree, app, arg, list, TreeParser)
import Control.Applicative
import Html.Tree
import CCO.Feedback
import Bibtex.Basic
import Data.List (sortBy)
import Data.Maybe (fromJust)

-- | A bibtex database is a list of 
data BibtexDb
    = BibtexDb [BibtexEntry]
    deriving (Show, Eq)

data BibtexEntry
    = Entry Type Key [(Field, String)]
    deriving (Show, Eq)


--------------------------------------------------------------------------------
--- ATerm
--------------------------------------------------------------------------------

instance Tree BibtexDb where
    fromTree (BibtexDb es) = App "BibtexDb" [fromTree es]
    toTree = parseTree [app "BibtexDb" (BibtexDb <$> arg)]

instance Tree BibtexEntry where
    fromTree (Entry t k fv) = App "Entry" [fromTree t, fromTree k, fromTree fv]
    toTree = parseTree [app "Entry" (Entry <$> arg <*> arg <*> arg)]
