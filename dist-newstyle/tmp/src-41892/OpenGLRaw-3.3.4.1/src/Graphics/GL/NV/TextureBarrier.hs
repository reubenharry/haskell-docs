--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureBarrier
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureBarrier (
  -- * Extension Support
  glGetNVTextureBarrier,
  gl_NV_texture_barrier,
  -- * Functions
  glTextureBarrierNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
