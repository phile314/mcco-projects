{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

-- | Internal functions for the Bibtex module. Contains Template Haskell code,
--   as template haskell functions cannot be applied in the file where they are defined.
module Bibtex.Internal where

import Language.Haskell.TH


-- | Returns a list of all simple constructors of a type.
--   Only constructors with no arguments are considered simple.
getSimpleCTors :: Name -> ExpQ
getSimpleCTors name = do
    (TyConI (DataD _ _ _ cs _)) <- reify name
    let cs' = filter isSimple cs

    listE $ map (\(NormalC n _) -> conE n) cs'

    where isSimple (NormalC _ []) = True
          isSimple _              = False
