{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.Fence
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.Fence (
  -- * Extension Support
  glGetAPPLEFence,
  gl_APPLE_fence,
  -- * Enums
  pattern GL_DRAW_PIXELS_APPLE,
  pattern GL_FENCE_APPLE,
  -- * Functions
  glDeleteFencesAPPLE,
  glFinishFenceAPPLE,
  glFinishObjectAPPLE,
  glGenFencesAPPLE,
  glIsFenceAPPLE,
  glSetFenceAPPLE,
  glTestFenceAPPLE,
  glTestObjectAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
