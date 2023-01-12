{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureSNorm
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureSNorm (
  -- * Extension Support
  glGetEXTTextureSNorm,
  gl_EXT_texture_snorm,
  -- * Enums
  pattern GL_ALPHA16_SNORM,
  pattern GL_ALPHA8_SNORM,
  pattern GL_ALPHA_SNORM,
  pattern GL_INTENSITY16_SNORM,
  pattern GL_INTENSITY8_SNORM,
  pattern GL_INTENSITY_SNORM,
  pattern GL_LUMINANCE16_ALPHA16_SNORM,
  pattern GL_LUMINANCE16_SNORM,
  pattern GL_LUMINANCE8_ALPHA8_SNORM,
  pattern GL_LUMINANCE8_SNORM,
  pattern GL_LUMINANCE_ALPHA_SNORM,
  pattern GL_LUMINANCE_SNORM,
  pattern GL_R16_SNORM,
  pattern GL_R8_SNORM,
  pattern GL_RED_SNORM,
  pattern GL_RG16_SNORM,
  pattern GL_RG8_SNORM,
  pattern GL_RGB16_SNORM,
  pattern GL_RGB8_SNORM,
  pattern GL_RGBA16_SNORM,
  pattern GL_RGBA8_SNORM,
  pattern GL_RGBA_SNORM,
  pattern GL_RGB_SNORM,
  pattern GL_RG_SNORM,
  pattern GL_SIGNED_NORMALIZED
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
