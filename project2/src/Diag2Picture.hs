module Diag2Picture (
  toPicture
) where

import qualified Diag2Picture.Render as R
import qualified Diag2Picture.Expand as E
import CCO.Picture
import CCO.Diag


toPicture :: Diag -> Picture
toPicture = R.foldTree . E.foldTree
