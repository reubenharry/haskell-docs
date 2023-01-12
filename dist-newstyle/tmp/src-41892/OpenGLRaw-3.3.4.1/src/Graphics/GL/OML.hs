--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OML
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing OML extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.OML (
  module Graphics.GL.OML.Interlace,
  module Graphics.GL.OML.Resample,
  module Graphics.GL.OML.Subsample
) where

import Graphics.GL.OML.Interlace
import Graphics.GL.OML.Resample
import Graphics.GL.OML.Subsample
