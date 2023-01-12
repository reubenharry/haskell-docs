{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.DepthBufferFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.DepthBufferFloat (
  -- * Extension Support
  glGetNVDepthBufferFloat,
  gl_NV_depth_buffer_float,
  -- * Enums
  pattern GL_DEPTH32F_STENCIL8_NV,
  pattern GL_DEPTH_BUFFER_FLOAT_MODE_NV,
  pattern GL_DEPTH_COMPONENT32F_NV,
  pattern GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV,
  -- * Functions
  glClearDepthdNV,
  glDepthBoundsdNV,
  glDepthRangedNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
