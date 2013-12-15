-- | This module contains functions and data types related to errors.

module Type.Error (
    TypeError(..)
  , ScopeError(..)
  ) where

import CCO.Feedback (Message(..))
import CCO.Printing 
import CCO.SourcePos (SourcePos(..), Pos(..), Source(..))
import Type.Internal

-- | Represent a type error. The first type is the actual given type.
-- The second is a list containing expected types.
data TypeError = PlatformError Type 
               | ProgramError Type 
               | CompilerError Type
               | LangError Type Type

data ScopeError = UndefinedVariable String

instance Printable TypeError where
  pp e = indent 4 msg 
    where msg = text "Type Error: " >|< describe e >-< exp >-< act
          exp = text "Expected: " >|< expected e
          act = text "Actual: " >|< actual e

instance Printable ScopeError where
  pp (UndefinedVariable v) = indent 4 msg
    where msg = text "Scope Error: " >|< what
          what = text $ "`" ++ v ++ "' is not in scope" 

instance Printable SourcePos where
  pp (SourcePos f p) = text $ format f ++ ":" ++ format' p
    where format Stdin = "stdin"
          format (File f) = f
          format' EOF = "end of file"
          format' (Pos l c) = show l ++ ":" ++ show c ++ ":"

-- | Returns a 'Doc' containing a general description of each error.
describe :: TypeError -> Doc
describe (PlatformError _) = text "Invalid platform or interpreter given"
describe (ProgramError _)  = text "Invalid program given"
describe (CompilerError _) = text "Invalid compiler given"
describe (LangError _ _)   = text "Language mismatch"

-- | Returns a 'Doc' representing the expected type for each error
expected :: TypeError -> Doc 
expected (PlatformError _) = text "Platform or Interpreter"
expected (ProgramError _ ) = text "Program"
expected (CompilerError _) = text "Compiler"
expected (LangError t1 (PlatformT _)) = readable $ PlatformT (language t1)
expected (LangError t1 (ProgramT m (PlatformT _))) = readable $ ProgramT m (PlatformT (language t1))
expected (LangError t1 (ProgramT m (CompilerT _ l2))) = readable $ ProgramT m (CompilerT (language t1) l2)

-- | Returns a 'Doc' representing the actual type for each error
actual :: TypeError -> Doc
actual (PlatformError t) = readable t
actual (ProgramError t) = readable t
actual (CompilerError t) = readable t
actual (LangError t1 t2) = readable t2

-- | Returns a 'Doc' containing a user-readable representation of a type.
-- Fails for types non exposed to the user.
readable (PlatformT l) = text $ "Platform _ " ++ l
readable (ProgramT m (PlatformT l)) = text $ unwords ["Interpreter _", m, l]
readable (ProgramT m (CompilerT l1 l2)) = text $ unwords ["Compiler _", l1, l2, m]
readable (ProgramT l _) = text $ "Program _ " ++ l
readable _ = error "Non-exposed type"
