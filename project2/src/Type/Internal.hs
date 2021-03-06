-- | This module defines the data type used to encode t-diagrams types

module Type.Internal where

data Type 
  -- | The type of a program t-diagram. 
  = ProgramT { language :: String,  -- ^ the implementation language
               result :: Type }     -- ^ the resulting type when the program is run
  -- | The type of platform t-diagram. 
  | PlatformT { language :: String } -- ^ the implementation language
  -- | The typeof a compiler t-diagram.
  | CompilerT { from :: String, -- ^ the source language,
                to :: String } -- ^ the target language,
  -- | A dummy type used for t-diagram that are completely executed.
  | UnitT
  -- | A type used to encode ill-typed t-diagrams.
  | ErrorT
  deriving (Eq, Show)
