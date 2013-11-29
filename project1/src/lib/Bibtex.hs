{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

-- | This module defines the data types 'Type' and 'Field' used
-- inside a 'BibtexEntry'.

module Bibtex
    ( BibtexDb (..)
    , BibtexEntry (..)
    , Key
    , Field (..)
    , Type (..)
    , fields
    , types)
where

import CCO.Tree
import CCO.Tree.Parser
import Control.Applicative
import Bibtex.Internal
import CCO.Tree (ATerm)

-- | A bibtex database is a list of `BibtexEntry`. 
data BibtexDb
    = BibtexDb [BibtexEntry]
    deriving (Show, Eq)

-- | A bibtex entry.
data BibtexEntry
    = Entry Type Key [(Field, String)]
    deriving (Show, Eq)

-- | Represents a 'BibtexEntry' type.
data Type
  = Article
  | Book
  | Booklet
  | Conference
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | Mastersthesis
  | Misc
  | Phdthesis
  | Proceedings
  | Techreport
  | Unpublished
  | UnknownType String
    deriving (Show, Eq, Ord)

-- | Represents a bibtex entry's key.
type Key = String

-- | Represents a field that can be possibly used in a 'BibtexEntry'.
data Field
    = Address
    | Annote
    | Author
    | Booktitle
    | Chapter
    | Crossref
    | Edition
    | Editor
    | HowPublished
    | Institution
    | Journal
    | Key
    | Month
    | Note
    | Number
    | Organization
    | Pages
    | Publisher
    | School
    | Series
    | Title
    | Type
    | Volume
    | Year
    | UnknownField String
    deriving (Show, Eq, Ord)

-- We have to resort to template haskell for getting the list of constructors because
-- it is not possible to derive Enum from a type where at least one constructor takes an argument.

-- | A list containing all the 'Field' constructors except `UnkownField`.
fields :: [Field]
fields = $(getSimpleCTors ''Field)

-- | A list containing all the 'Type' constructors except `UnkownType`.
types :: [Type]
types = $(getSimpleCTors ''Type)

--------------------------------------------------------------------------------
----- ATerm
----------------------------------------------------------------------------------

instance Tree BibtexDb where
    fromTree (BibtexDb es) = App "BibtexDb" [fromTree es]
    toTree = parseTree [app "BibtexDb" (BibtexDb <$> arg)]

instance Tree BibtexEntry where
    fromTree (Entry t k fv) = App "Entry" [fromTree t, fromTree k, fromTree fv]
    toTree = parseTree [app "Entry" (Entry <$> arg <*> arg <*> arg)]

instance Tree Field where
    fromTree (UnknownField f) = App "UnknownField" [fromTree f]
    fromTree f = App (show f) []
    toTree = parseTree (pUnk:ps)
        where ps = map pCons fields
              pUnk = app "UnknownField" (UnknownField <$> arg)

instance Tree Type where
    fromTree (UnknownType t) = App "UnknownType" [fromTree t]
    fromTree t = App (show t) []
    toTree = parseTree (pUnk:ps)
        where ps = map pCons types
              pUnk = app "UnknownType" (UnknownType <$> arg)

-- | A 'TreeParser' for single showable constructors.
pCons :: (Show a, Tree a) => a -> TreeParser a
pCons c = app (show c) (pure c)
