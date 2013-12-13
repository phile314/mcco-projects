-- | The program tc-tdiag

module Main (main) where

import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Diag       (Diag)
import CCO.Tree       (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow  ((>>>), arr)
import Type           (typeCheck)

-- | The entry point of the program tc-tdiag
main :: IO ()
main = ioWrap $
  parser >>> (component toTree :: Component ATerm Diag) >>> 
  component typeCheck >>> arr fromTree >>> printer 
