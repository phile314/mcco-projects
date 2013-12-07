-- | This module defines the data type used to encode t-diagrams types

module Type.Internal where

data Type 
  -- | The type of a program t-diagram. 
  = ProgramT { language :: String } -- ^ the implementation language
  -- | The type of platform t-diagram. 
  | PlatformT { language :: String } -- ^ the implementation language
  -- | The type of an interpreter t-diagram.
  | Interpreter {target :: String, -- ^ the implementation language 
                 language :: String} -- ^ the language being interpreted.
  -- | The typeof a compiler t-diagram.
  | Compiler { from :: String, -- ^ the source language,
               to :: String, -- ^ the target language,
               language :: String} -- ^ the implementation language of the compiler.
  -- | A dummy type used for t-diagram that are completely executed.
  | Unit

