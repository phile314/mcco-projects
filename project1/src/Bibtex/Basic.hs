module Bibtex.Basic
    ( Key
    , Field (..)
    , Type (..)
    , fields
    , types )
where

import CCO.Tree
import CCO.Tree.Parser
import Control.Applicative

type Key = String

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
--    | Unknown fieldName :: String
    deriving (Show, Eq, Enum, Bounded)

data Type
  = Article
  | Book
  | Booklet
  | Conference
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | Masterthesis
  | Misc
  | Phdthesis
  | Proceedings
  | Techreport
  | Unpublished
    deriving (Show, Eq, Enum, Bounded)

-- | A list containing all the 'Field' constructors
fields :: [Field]
fields = enumFromTo minBound maxBound

-- | A list containing all the 'Type' constructors
types :: [Type]
types = enumFromTo minBound maxBound


--------------------------------------------------------------------------------
----- ATerm
----------------------------------------------------------------------------------

instance Tree Field where
    fromTree f = App (show f) []
    -- should we use the parser stuff here too?
    toTree = parseTree ps
        where ps = map pCons (fields)


instance Tree Type where
    fromTree t = App (show t) []
    toTree = parseTree ps
        where ps = map pCons (types)

-- | A 'TreeParser' for single showable constructors.
pCons :: (Show a, Tree a) => a -> TreeParser a
pCons c = app (show c) (pure c)
