{-# LANGUAGE FlexibleInstances #-}

module Bibtex.Basic
    ( Key
    , Field (..)
    , Type (..)
    , fields
    , types
    , fieldOrd)
where

import CCO.Tree
import CCO.Tree.Parser
import Control.Applicative
import Control.Monad (ap)
import Data.List (elemIndex)

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


fieldOrd :: Type -> (Field -> Field -> Ordering)
fieldOrd etype = \f1 f2 -> elemIndex f1 fo `compare` elemIndex f2 fo
    where fo = fieldOrder $ spec etype


data Spec
    = AllOf     [Spec]
    | OneOf     [Spec]
    | SomeOf    [Spec]
    | Optional  Spec
    | SField    Field

class SpecCollector a where
    coll1 :: ([Spec] -> Spec) -> [Spec] -> a
    coll :: ([Spec] -> Spec) -> a
    coll f = coll1 f []
instance SpecCollector Spec where
    coll1 f acc = f acc
instance (SpecDef a, SpecCollector r) => SpecCollector (a -> r) where
    coll1 f acc = \x -> coll1 f ((conv x):acc)

class SpecDef a where
    conv :: a -> Spec
instance SpecDef Field where
    conv f = field f
instance SpecDef Spec where
    conv = id

allOf   :: SpecCollector a => a
allOf   = coll AllOf
someOf  :: SpecCollector a => a
someOf  = coll SomeOf
oneOf   :: SpecCollector a => a
oneOf   = coll OneOf
opt     :: SpecDef a => a -> Spec
opt     = Optional . conv
field   = SField


sAE :: Spec
sAE = oneOf Author Editor

spec :: Type -> Spec
spec Article        = allOf sAE Title Journal Year (opt Volume) (opt Number) (opt Pages) (opt Month) (opt Note)
spec Book           = allOf sAE Title Publisher (opt (oneOf Volume Number :: Spec)) (opt Series) (opt Address) (opt Edition) (opt Month) (opt Note)
spec Booklet        = allOf Title
spec Conference     = spec Inproceedings
spec Inbook         = allOf sAE (oneOf Chapter Pages :: Spec) Publisher Year
{-spec Incollection   = 
spec Inproceedings  =
spec Manual         =
spec Masterthesis   =
spec Misc           =
spec Phdthesis      =
spec Proceedings    =
spec Techreport     =
spec Unpublished    =-}
spec _ = undefined

fieldOrder :: Spec -> [Field]
fieldOrder (AllOf ss)   = concatMap fieldOrder ss
fieldOrder (OneOf ss)   = concatMap fieldOrder ss
fieldOrder (SomeOf ss)  = concatMap fieldOrder ss
fieldOrder (Optional s) = fieldOrder s
fieldOrder (SField f)   = [f]

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
