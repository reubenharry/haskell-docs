--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.HP
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing HP extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.HP (
  module Graphics.GL.HP.ConvolutionBorderModes,
  module Graphics.GL.HP.ImageTransform,
  module Graphics.GL.HP.OcclusionTest,
  module Graphics.GL.HP.TextureLighting
) where

import Graphics.GL.HP.ConvolutionBorderModes
import Graphics.GL.HP.ImageTransform
import Graphics.GL.HP.OcclusionTest
import Graphics.GL.HP.TextureLighting
