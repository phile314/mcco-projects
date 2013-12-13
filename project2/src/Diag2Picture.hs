module Diag2Picture (
  toPicture
) where

import Diag2Picture.AG
import CCO.Picture
import CCO.Diag


toPicture :: Diag -> Picture
toPicture d = Picture (0,0) $ pict_Syn_Diag $ wrap_Diag (sem_Diag d) Inh_Diag
