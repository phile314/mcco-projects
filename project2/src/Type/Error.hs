-- | This module contains functions and data types related to errors.

module Type.Error (
    TypeError(..)
  , ScopeError(..)
  , executeError
  , compileError
  , scopeError
  ) where

import CCO.Feedback (Message(..))
import CCO.Printing 
import CCO.SourcePos (SourcePos(..), Pos(..), Source(..))
import Type.Internal

-- | Represent a type error. 
data TypeError = PlatformError { t :: Type, pos :: SourcePos } -- ^ The given type is not a platform
               | ProgramError { t :: Type, pos :: SourcePos }  -- ^ The given type is not a program
               | CompilerError { t :: Type, pos :: SourcePos } -- ^ The given type is not a compiler
               | LangError { t1 :: Type, t2 :: Type, pos :: SourcePos } -- ^ The languages of the types given do not match

-- | Represents a error related to scope.
data ScopeError = UndefinedVariable String SourcePos

compileError :: SourcePos -> TypeError -> Message
compileError (SourcePos _ pos) e = Error $ errorMessage e >-< indent 8 descr
  where descr = text "In `compile' (at " >|< pp pos >|< text ")"

executeError :: SourcePos -> TypeError -> Message
executeError (SourcePos _ pos) e = Error $ errorMessage e >-< indent 8 descr
  where descr = text "In `execute' (at " >|< pp pos >|< text ")"

scopeError :: ScopeError -> Message
scopeError = Error . errorMessage

-- | @'errorMessage' p e@ produces an error message at position @p@ for the 'Printable' datatype @e@.
errorMessage :: (Position a, Printable a) => a -> Doc
errorMessage e = pp (positionOf e) >-< pp e

class Position a where
  -- | Returns the 'SourcePos' of an object
  positionOf :: a -> SourcePos

instance Position TypeError where
  positionOf = pos

instance Position ScopeError where
  positionOf (UndefinedVariable _ p) = p

instance Printable TypeError where
  pp e = indent 4 msg 
    where msg = text "Type Error: " >|< describe e >-< exp >-< act
          exp = text "Expected: " >|< expected e
          act = text "Actual: " >|< actual e

instance Printable ScopeError where
  pp (UndefinedVariable v p) = indent 4 msg
    where msg = text "Scope Error: " >|< what
          what = text $ "`" ++ v ++ "' is not in scope" 

instance Printable SourcePos where
  pp (SourcePos f p) = text (format f ++ ":") >|< pp p
    where format Stdin = "stdin"
          format (File f) = f

instance Printable Pos where
  pp EOF = text "end of file"
  pp (Pos l c) = text $ show l ++ ":" ++ show c

-- | Returns a 'Doc' containing a general description of each error.
describe :: TypeError -> Doc
describe (PlatformError _ _) = text "Invalid platform or interpreter given"
describe (ProgramError _ _)  = text "Invalid program given"
describe (CompilerError _ _) = text "Invalid compiler given"
describe (LangError _ _ _)   = text "Language mismatch"

-- | Returns a 'Doc' representing the expected type for each error
expected :: TypeError -> Doc 
expected (PlatformError _ _) = text "Platform or Interpreter"
expected (ProgramError _ _) = text "Program"
expected (CompilerError _ _) = text "Compiler"
expected (LangError t1 (PlatformT _ ) _) = readable $ PlatformT (language t1)
expected (LangError t1 (ProgramT m (PlatformT _)) _) = readable $ ProgramT m (PlatformT (language t1))
expected (LangError t1 (ProgramT m (CompilerT _ l2)) _) = readable $ ProgramT m (CompilerT (language t1) l2)

-- | Returns a 'Doc' representing the actual type for each error
actual :: TypeError -> Doc
actual (LangError _ t2 _) = readable t2
actual e  = readable (t e)

-- | Returns a 'Doc' containing a user-readable representation of a type.
-- Fails for types non exposed to the user.
readable (PlatformT l) = text $ "Platform " ++ l
readable (ProgramT m (PlatformT l)) = text $ unwords ["Interpreter _", "for", l, "in", m]
readable (ProgramT m (CompilerT l1 l2)) = text $ unwords ["Compiler _", l1, l2, m]
readable (ProgramT l _) = text $ l ++ " Program" 
readable _ = error "Non-exposed type"
