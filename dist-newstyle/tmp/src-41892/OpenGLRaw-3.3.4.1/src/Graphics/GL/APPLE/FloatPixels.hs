{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.FloatPixels
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.FloatPixels (
  -- * Extension Support
  glGetAPPLEFloatPixels,
  gl_APPLE_float_pixels,
  -- * Enums
  pattern GL_ALPHA_FLOAT16_APPLE,
  pattern GL_ALPHA_FLOAT32_APPLE,
  pattern GL_COLOR_FLOAT_APPLE,
  pattern GL_HALF_APPLE,
  pattern GL_INTENSITY_FLOAT16_APPLE,
  pattern GL_INTENSITY_FLOAT32_APPLE,
  pattern GL_LUMINANCE_ALPHA_FLOAT16_APPLE,
  pattern GL_LUMINANCE_ALPHA_FLOAT32_APPLE,
  pattern GL_LUMINANCE_FLOAT16_APPLE,
  pattern GL_LUMINANCE_FLOAT32_APPLE,
  pattern GL_RGBA_FLOAT16_APPLE,
  pattern GL_RGBA_FLOAT32_APPLE,
  pattern GL_RGB_FLOAT16_APPLE,
  pattern GL_RGB_FLOAT32_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
