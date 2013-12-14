-- | This module tests the type checker for t-diagrams

import CCO.Diag
import Type.Internal (Type (..))
import Type.AG
import System.Exit (exitFailure)
import Test.QuickCheck.Property (Property, property)
import Test.QuickCheck.Test (isSuccess, quickCheckResult)
import Utility (arbitrary)

-- | The tests that will be run
tests :: [Property]
tests = [property correctType, property wrongExecute, property wrongCompile]

-- | Returns the type of a 'Diag'.
typeOf :: Diag -> Type
typeOf d = ty_Syn_Diag $ wrap_Diag (sem_Diag d) (Inh_Diag {})

-- | Tests that the correct type is returned.
-- It does *not* test that the t-diagram is well-typed.
correctType :: Diag -> Bool
correctType d@(Diag p td) = (expected td) == (typeOf d)
  where expected (Platform m) = PlatformT m 
        expected (Program _ l) = ProgramT l UnitT 
        expected (Interpreter _ l m) = ProgramT m (InterpreterT l)
        expected (Compiler _ l1 l2 m) = ProgramT m (CompilerT l1 l2)
        expected (Execute d1 d2) = result $ typeOf d1
        expected (Compile d1 d2) = ProgramT ((to . result . typeOf) d2) ((result . typeOf) d1)

-- | Tests that ill-typed expression combined with compile return a 'TypeError',
wrongCompile :: Type -> Type -> Bool
wrongCompile (ProgramT l r) (ProgramT m (CompilerT l1 l2)) | l == l1 = True
wrongCompile t1@(ProgramT l r) t2 = compile t1 t2 == (Left $ TypeError t2 [ProgramT "<m>" (CompilerT l "<l2>")])
wrongCompile t1 t2 = compile t1 t2 == (Left $ TypeError t1 [ProgramT "<l>" UnitT])

-- | Tests that ill-typed expression combined with execute return a 'TypeError',
wrongExecute :: Type -> Type -> Bool
wrongExecute (ProgramT l r) (PlatformT l') | l == l' = True
wrongExecute (ProgramT l r) (ProgramT m (InterpreterT l')) | l == l' = True
wrongExecute t1@(ProgramT l r) t2 = 
  execute t1 t2 == (Left $ TypeError t2 [PlatformT l, InterpreterT l])
wrongExecute t1 t2 = execute t1 t2 == (Left $ TypeError t1 [ProgramT "<l>" UnitT])

-- | The entry point of the testsuite
main :: IO ()
main = do
  results <- mapM quickCheckResult tests
  if all isSuccess results
    then return()
    else exitFailure

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | Checks whether a type is an error type, e.g. 'ErrorT'.
isErrorT ErrorT = True
isErrorT _ = False
