-- | This module defines convenience functions and instances for testing.

module Utility ( arbitrary ) where

import CCO.SourcePos (SourcePos(..), Source(..), Pos(..))
import CCO.Diag (Diag_(..), Diag(..))
import Control.Monad (liftM, liftM2)
import qualified Type.Internal as T (Type(..))
import Type.Internal (Type)
import Test.QuickCheck.Gen (Gen, elements, suchThat, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

instance Arbitrary Diag_ where
  arbitrary = oneof [program, platform, compiler, interpreter, execute, compile]

instance Arbitrary Diag where
  arbitrary = liftM diag arbitrary

-- | A list of platforms. This can be also considered implementation language.
platforms :: [String]
platforms = ["i686-linux", "i686-windows", "amd64-linux", 
             "i686-darwin", "ppc-darwin", "JVM"]

-- | A list of high level languages.
highLanguages :: [String]
highLanguages = ["C", "Java", "Python", "Haskell", "OCaml", "Erlang"]

-- | A simple generator for implementation languages
language :: Gen String
language = elements $ highLanguages ++ platforms

-- | A simple generator for programs
program :: Gen Diag_
program = liftM (Program "p") language

-- | A simple generator for platforms
platform :: Gen Diag_
platform = liftM Platform (elements platforms)

-- | A simple generator for interpreter
interpreter :: Gen Diag_
interpreter = do
  l <- elements highLanguages
  m <- elements platforms
  return $ Interpreter (l ++ "_on_" ++ m) l m

-- | A simple generator for compiler from high languages to low level languages.
compiler :: Gen Diag_
compiler = do
  l1 <- elements highLanguages
  l2 <- elements platforms
  m <- elements platforms
  return $ Compiler (l1 ++ "_to_" ++ l2 ++ "_on_" ++ m) l1 l2 m

-- | A generator for simple valid 'Execute' objects.
-- Only plain programs are executed.
execute :: Gen Diag_
execute = do
  pl@(Platform m) <- platform
  pr <- suchThat program (\(Program _ l) -> m == l)
  return $ Execute (diag pr) (diag pl)

-- | A generator for simple valid 'Compile' objects.
-- Only plain programs are compiled.
compile :: Gen Diag_
compile = do
  c@(Compiler _ l1 _ _) <- compiler
  p <- suchThat program (\(Program _ l) -> l == l1)
  return $ Compile (diag p) (diag c)

-- | A dummy constructor for 'Diag'. 
-- Since we are genereting t-diagrams instead of parsing them the source position
-- field contains a fixed value.
diag :: Diag_ -> Diag
diag d = Diag (SourcePos Stdin (Pos 0 0)) d

-------------------------------------------------------------------------------
-- Type Generator
-------------------------------------------------------------------------------

instance Arbitrary Type where
  arbitrary = oneof [simple, complex]

complex :: Gen Type
complex = complex' 10
  where complex' 0 = simple
        complex' n = oneof [liftM2 T.ProgramT language (complex' (n/2)), simple]

simple :: Gen Type
simple = oneof [basic, platformT, interpreterT, compilerT]
  where basic = elements [T.UnitT, T.ErrorT]
  
platformT :: Gen Type
platformT = elements platforms >>= return . T.PlatformT

interpreterT :: Gen Type
interpreterT = language >>= return . T.InterpreterT

compilerT :: Gen Type
compilerT = do
  l1 <- elements highLanguages
  l2 <- elements platforms
  return (T.CompilerT l1 l2)
