module Diag2Picture (
  toPicture
) where

import Diag2Picture.AG
import CCO.Picture
import CCO.Diag


toPicture :: Diag -> Picture
toPicture d = foldTree d
