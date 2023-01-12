{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FloatBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FloatBuffer (
  -- * Extension Support
  glGetNVFloatBuffer,
  gl_NV_float_buffer,
  -- * Enums
  pattern GL_FLOAT_CLEAR_COLOR_VALUE_NV,
  pattern GL_FLOAT_R16_NV,
  pattern GL_FLOAT_R32_NV,
  pattern GL_FLOAT_RG16_NV,
  pattern GL_FLOAT_RG32_NV,
  pattern GL_FLOAT_RGB16_NV,
  pattern GL_FLOAT_RGB32_NV,
  pattern GL_FLOAT_RGBA16_NV,
  pattern GL_FLOAT_RGBA32_NV,
  pattern GL_FLOAT_RGBA_MODE_NV,
  pattern GL_FLOAT_RGBA_NV,
  pattern GL_FLOAT_RGB_NV,
  pattern GL_FLOAT_RG_NV,
  pattern GL_FLOAT_R_NV,
  pattern GL_TEXTURE_FLOAT_COMPONENTS_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
