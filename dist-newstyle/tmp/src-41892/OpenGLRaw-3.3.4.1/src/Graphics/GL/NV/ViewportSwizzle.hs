{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ViewportSwizzle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ViewportSwizzle (
  -- * Extension Support
  glGetNVViewportSwizzle,
  gl_NV_viewport_swizzle,
  -- * Enums
  pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_W_NV,
  pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_X_NV,
  pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Y_NV,
  pattern GL_VIEWPORT_SWIZZLE_NEGATIVE_Z_NV,
  pattern GL_VIEWPORT_SWIZZLE_POSITIVE_W_NV,
  pattern GL_VIEWPORT_SWIZZLE_POSITIVE_X_NV,
  pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Y_NV,
  pattern GL_VIEWPORT_SWIZZLE_POSITIVE_Z_NV,
  pattern GL_VIEWPORT_SWIZZLE_W_NV,
  pattern GL_VIEWPORT_SWIZZLE_X_NV,
  pattern GL_VIEWPORT_SWIZZLE_Y_NV,
  pattern GL_VIEWPORT_SWIZZLE_Z_NV,
  -- * Functions
  glViewportSwizzleNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
