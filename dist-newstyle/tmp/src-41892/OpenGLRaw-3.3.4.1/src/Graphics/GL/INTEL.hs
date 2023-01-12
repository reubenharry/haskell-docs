--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing INTEL extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL (
  module Graphics.GL.INTEL.BlackholeRender,
  module Graphics.GL.INTEL.ConservativeRasterization,
  module Graphics.GL.INTEL.FramebufferCmaa,
  module Graphics.GL.INTEL.MapTexture,
  module Graphics.GL.INTEL.ParallelArrays,
  module Graphics.GL.INTEL.PerformanceQuery
) where

import Graphics.GL.INTEL.BlackholeRender
import Graphics.GL.INTEL.ConservativeRasterization
import Graphics.GL.INTEL.FramebufferCmaa
import Graphics.GL.INTEL.MapTexture
import Graphics.GL.INTEL.ParallelArrays
import Graphics.GL.INTEL.PerformanceQuery
