-- | Converts diagrams to pictures.
module Diag2Picture (
  toPicture
) where

import qualified Diag2Picture.Render as R
import qualified Diag2Picture.Inline as I
import CCO.Picture
import CCO.Diag

-- | Converts a diagram to pictures. Always succeeds
--   if the given diagram does type check.
toPicture :: Diag -> Picture
toPicture = R.render . I.inline
