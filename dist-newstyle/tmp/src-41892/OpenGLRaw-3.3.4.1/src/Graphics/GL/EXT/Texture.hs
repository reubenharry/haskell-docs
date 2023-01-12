{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.Texture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.Texture (
  -- * Extension Support
  glGetEXTTexture,
  gl_EXT_texture,
  -- * Enums
  pattern GL_ALPHA12_EXT,
  pattern GL_ALPHA16_EXT,
  pattern GL_ALPHA4_EXT,
  pattern GL_ALPHA8_EXT,
  pattern GL_INTENSITY12_EXT,
  pattern GL_INTENSITY16_EXT,
  pattern GL_INTENSITY4_EXT,
  pattern GL_INTENSITY8_EXT,
  pattern GL_INTENSITY_EXT,
  pattern GL_LUMINANCE12_ALPHA12_EXT,
  pattern GL_LUMINANCE12_ALPHA4_EXT,
  pattern GL_LUMINANCE12_EXT,
  pattern GL_LUMINANCE16_ALPHA16_EXT,
  pattern GL_LUMINANCE16_EXT,
  pattern GL_LUMINANCE4_ALPHA4_EXT,
  pattern GL_LUMINANCE4_EXT,
  pattern GL_LUMINANCE6_ALPHA2_EXT,
  pattern GL_LUMINANCE8_ALPHA8_EXT,
  pattern GL_LUMINANCE8_EXT,
  pattern GL_PROXY_TEXTURE_1D_EXT,
  pattern GL_PROXY_TEXTURE_2D_EXT,
  pattern GL_REPLACE_EXT,
  pattern GL_RGB10_A2_EXT,
  pattern GL_RGB10_EXT,
  pattern GL_RGB12_EXT,
  pattern GL_RGB16_EXT,
  pattern GL_RGB2_EXT,
  pattern GL_RGB4_EXT,
  pattern GL_RGB5_A1_EXT,
  pattern GL_RGB5_EXT,
  pattern GL_RGB8_EXT,
  pattern GL_RGBA12_EXT,
  pattern GL_RGBA16_EXT,
  pattern GL_RGBA2_EXT,
  pattern GL_RGBA4_EXT,
  pattern GL_RGBA8_EXT,
  pattern GL_TEXTURE_ALPHA_SIZE_EXT,
  pattern GL_TEXTURE_BLUE_SIZE_EXT,
  pattern GL_TEXTURE_GREEN_SIZE_EXT,
  pattern GL_TEXTURE_INTENSITY_SIZE_EXT,
  pattern GL_TEXTURE_LUMINANCE_SIZE_EXT,
  pattern GL_TEXTURE_RED_SIZE_EXT,
  pattern GL_TEXTURE_TOO_LARGE_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
