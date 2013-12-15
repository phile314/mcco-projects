-- | This module tests the type checker for t-diagrams

import CCO.Diag
import Type.Internal (Type (..))
import Type.Error (TypeError(..))
import Type.AG
import System.Exit (exitFailure)
import Test.QuickCheck.Property (Property, property, forAll)
import Test.QuickCheck.Test (isSuccess, quickCheckResult)
import Utility (arbitrary, illGenerator)
import qualified Data.Map as M

-- | The tests that will be run
tests :: [Property]
tests = [property correctType, forAll illGenerator illTyped]

-- | Returns the 'Type' of the given 'Diag'.
-- If the t-diagram is ill-typed 'typeOf' fails.
-- The t-diagram cannot contain any variable because this environment information
-- is not available.
typeOf :: Diag -> Type
typeOf d = ty_Syn_Diag $ wrap_Diag (sem_Diag d) (Inh_Diag {env_Inh_Diag = M.empty})

-- | Tests that the correct type is returned.
-- It does *not* test that the t-diagram is well-typed.
correctType :: Diag -> Bool
correctType d@(Diag p td) = if (not $ (expected td) == (typeOf d)) then (error . unwords) (map show [(expected td), (typeOf d)]) else True
  where expected (Platform m) = PlatformT m 
        expected (Program _ l) = ProgramT l UnitT 
        expected (Interpreter _ l m) = ProgramT m (PlatformT l)
        expected (Compiler _ l1 l2 m) = ProgramT m (CompilerT l1 l2)
        expected (Execute d1 (Diag _ (Platform _))) = result $ typeOf d1
        expected (Execute d1 d2) = ProgramT ((language . result . typeOf) d2) ((result . typeOf) d1)
        expected (Compile d1 d2) = ProgramT ((to . result . typeOf) d2) ((result . typeOf) d1)

-- | Checks that invalid compositions of t-diagrams produce an 'ErrorT'.
illTyped :: Diag -> Bool
illTyped d = typeOf d == ErrorT

-- | The entry point of the testsuite
main :: IO ()
main = do
  results <- mapM quickCheckResult tests
  if all isSuccess results
    then return()
    else exitFailure
