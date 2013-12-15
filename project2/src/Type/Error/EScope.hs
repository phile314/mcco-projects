-- | This module contains functions and data type for dealing with scope errors.

module Type.Error.EScope where

import CCO.Feedback (Message(..))
import CCO.Printing 
import CCO.SourcePos (SourcePos(..), Pos(..), Source(..))
import Type.Error.Internal
import Type.Internal

-- | Represents a error related to scope.
data ScopeError = UndefinedVariable String SourcePos

-- | Produces an 'Error' 'Message' for a 'ScopeError'.
scopeError :: ScopeError -> Message
scopeError = Error . errorMessage

instance Position ScopeError where
  positionOf (UndefinedVariable _ p) = p

instance Printable ScopeError where
  pp (UndefinedVariable v p) = indent 4 msg
    where msg = text "Scope Error: " >|< what
          what = text $ "`" ++ v ++ "' is not in scope" 
