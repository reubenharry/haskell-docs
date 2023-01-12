--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ThreeDFX
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing 3DFX extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.ThreeDFX (
  module Graphics.GL.ThreeDFX.Multisample,
  module Graphics.GL.ThreeDFX.Tbuffer,
  module Graphics.GL.ThreeDFX.TextureCompressionFXT1
) where

import Graphics.GL.ThreeDFX.Multisample
import Graphics.GL.ThreeDFX.Tbuffer
import Graphics.GL.ThreeDFX.TextureCompressionFXT1
