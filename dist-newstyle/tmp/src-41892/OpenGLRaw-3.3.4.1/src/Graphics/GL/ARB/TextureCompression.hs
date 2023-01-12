{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureCompression
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureCompression (
  -- * Extension Support
  glGetARBTextureCompression,
  gl_ARB_texture_compression,
  -- * Enums
  pattern GL_COMPRESSED_ALPHA_ARB,
  pattern GL_COMPRESSED_INTENSITY_ARB,
  pattern GL_COMPRESSED_LUMINANCE_ALPHA_ARB,
  pattern GL_COMPRESSED_LUMINANCE_ARB,
  pattern GL_COMPRESSED_RGBA_ARB,
  pattern GL_COMPRESSED_RGB_ARB,
  pattern GL_COMPRESSED_TEXTURE_FORMATS_ARB,
  pattern GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB,
  pattern GL_TEXTURE_COMPRESSED_ARB,
  pattern GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB,
  pattern GL_TEXTURE_COMPRESSION_HINT_ARB,
  -- * Functions
  glCompressedTexImage1DARB,
  glCompressedTexImage2DARB,
  glCompressedTexImage3DARB,
  glCompressedTexSubImage1DARB,
  glCompressedTexSubImage2DARB,
  glCompressedTexSubImage3DARB,
  glGetCompressedTexImageARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
