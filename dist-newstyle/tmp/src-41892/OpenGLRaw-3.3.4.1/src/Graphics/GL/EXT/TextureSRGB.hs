{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureSRGB
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureSRGB (
  -- * Extension Support
  glGetEXTTextureSRGB,
  gl_EXT_texture_sRGB,
  -- * Enums
  pattern GL_COMPRESSED_SLUMINANCE_ALPHA_EXT,
  pattern GL_COMPRESSED_SLUMINANCE_EXT,
  pattern GL_COMPRESSED_SRGB_ALPHA_EXT,
  pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT,
  pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT,
  pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT,
  pattern GL_COMPRESSED_SRGB_EXT,
  pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT,
  pattern GL_SLUMINANCE8_ALPHA8_EXT,
  pattern GL_SLUMINANCE8_EXT,
  pattern GL_SLUMINANCE_ALPHA_EXT,
  pattern GL_SLUMINANCE_EXT,
  pattern GL_SRGB8_ALPHA8_EXT,
  pattern GL_SRGB8_EXT,
  pattern GL_SRGB_ALPHA_EXT,
  pattern GL_SRGB_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
