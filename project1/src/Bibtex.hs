-- | This module defines the datatypes used to model a Bibtex database.

module Bibtex where

-- | A Bibtex database is a list of Bibtex Entry
type BibtexDb = [BibtexEntry]

-- | A Bibtex entry has a specific 'Type', a unique 'Key' and contains 
-- a list of 'Field' and related 'Value'
data BibtexEntry = Entry Type Key [(Field, Value)]
  deriving (Show, Eq)

-- | Represents the type of a Bibtex entry
type Type = String

-- | Represents a unique identifier
type Key = String

-- | Represents the name of a field
type Field = String

-- | Represents the content of a field
type Value = String
