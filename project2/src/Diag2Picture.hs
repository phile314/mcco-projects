module Diag2Picture (
  toPicture
) where

import qualified Diag2Picture.Render as R
import qualified Diag2Picture.Inline as I
import CCO.Picture
import CCO.Diag

toPicture :: Diag -> Picture
toPicture = R.render . I.inline
