-- | This module contains common functions, classes and instances.

module Type.Error.Internal where

import CCO.Printing 
import CCO.SourcePos (SourcePos(..), Pos(..), Source(..))

class Position a where
  -- | Returns the 'SourcePos' of an object
  positionOf :: a -> SourcePos

instance Printable Pos where
  pp EOF = text "end of file"
  pp (Pos l c) = text $ show l ++ ":" ++ show c

instance Printable SourcePos where
  pp (SourcePos f p) = text (format f ++ ":") >|< pp p
    where format Stdin = "stdin"
          format (File f) = f

-- | @'errorMessage' e@ produces an error message at position @p@ for the 
-- 'Printable' and 'Position' datatype @e@.
errorMessage :: (Position a, Printable a) => a -> Doc
errorMessage e = pp (positionOf e) >-< pp e
