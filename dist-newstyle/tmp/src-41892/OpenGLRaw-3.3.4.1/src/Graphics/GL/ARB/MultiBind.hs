--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.MultiBind
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.MultiBind (
  -- * Extension Support
  glGetARBMultiBind,
  gl_ARB_multi_bind,
  -- * Functions
  glBindBuffersBase,
  glBindBuffersRange,
  glBindImageTextures,
  glBindSamplers,
  glBindTextures,
  glBindVertexBuffers
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
