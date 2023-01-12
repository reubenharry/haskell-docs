--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureBarrier
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureBarrier (
  -- * Extension Support
  glGetARBTextureBarrier,
  gl_ARB_texture_barrier,
  -- * Functions
  glTextureBarrier
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
