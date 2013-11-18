-- | This module defines the datatypes used to model a Bibtex database.

module Bibtex where

-- | A Bibtex database is a list of Bibtex Entry
type BibtexDb = [BibtexEntry]

-- | A Bibtex entry has a specific type and contains some data
data BibtexEntry = Entry Type Data
  deriving (Show, Eq)

-- | Represents the type of a Bibtex entry
type Type = String

-- | An entry data is composed by a unique identifier ('Key') and
-- the related fields
data Data = Data Key [(Field, Value)]
  deriving (Show, Eq)

-- | Represents a unique identifier
type Key = String

-- | Represents the name of a field
type Field = String

-- | Represents the content of a field
type Value = String
