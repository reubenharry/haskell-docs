{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureFloat (
  -- * Extension Support
  glGetARBTextureFloat,
  gl_ARB_texture_float,
  -- * Enums
  pattern GL_ALPHA16F_ARB,
  pattern GL_ALPHA32F_ARB,
  pattern GL_INTENSITY16F_ARB,
  pattern GL_INTENSITY32F_ARB,
  pattern GL_LUMINANCE16F_ARB,
  pattern GL_LUMINANCE32F_ARB,
  pattern GL_LUMINANCE_ALPHA16F_ARB,
  pattern GL_LUMINANCE_ALPHA32F_ARB,
  pattern GL_RGB16F_ARB,
  pattern GL_RGB32F_ARB,
  pattern GL_RGBA16F_ARB,
  pattern GL_RGBA32F_ARB,
  pattern GL_TEXTURE_ALPHA_TYPE_ARB,
  pattern GL_TEXTURE_BLUE_TYPE_ARB,
  pattern GL_TEXTURE_DEPTH_TYPE_ARB,
  pattern GL_TEXTURE_GREEN_TYPE_ARB,
  pattern GL_TEXTURE_INTENSITY_TYPE_ARB,
  pattern GL_TEXTURE_LUMINANCE_TYPE_ARB,
  pattern GL_TEXTURE_RED_TYPE_ARB,
  pattern GL_UNSIGNED_NORMALIZED_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
