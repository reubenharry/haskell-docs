{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureCompressionS3TC
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureCompressionS3TC (
  -- * Extension Support
  glGetEXTTextureCompressionS3TC,
  gl_EXT_texture_compression_s3tc,
  -- * Enums
  pattern GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
  pattern GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
  pattern GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
  pattern GL_COMPRESSED_RGB_S3TC_DXT1_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
