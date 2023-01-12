--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing SUN extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN (
  module Graphics.GL.SUN.ConvolutionBorderModes,
  module Graphics.GL.SUN.GlobalAlpha,
  module Graphics.GL.SUN.MeshArray,
  module Graphics.GL.SUN.SliceAccum,
  module Graphics.GL.SUN.TriangleList,
  module Graphics.GL.SUN.Vertex
) where

import Graphics.GL.SUN.ConvolutionBorderModes
import Graphics.GL.SUN.GlobalAlpha
import Graphics.GL.SUN.MeshArray
import Graphics.GL.SUN.SliceAccum
import Graphics.GL.SUN.TriangleList
import Graphics.GL.SUN.Vertex
