{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.CompressedTexturePixelStorage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.CompressedTexturePixelStorage (
  -- * Extension Support
  glGetARBCompressedTexturePixelStorage,
  gl_ARB_compressed_texture_pixel_storage,
  -- * Enums
  pattern GL_PACK_COMPRESSED_BLOCK_DEPTH,
  pattern GL_PACK_COMPRESSED_BLOCK_HEIGHT,
  pattern GL_PACK_COMPRESSED_BLOCK_SIZE,
  pattern GL_PACK_COMPRESSED_BLOCK_WIDTH,
  pattern GL_UNPACK_COMPRESSED_BLOCK_DEPTH,
  pattern GL_UNPACK_COMPRESSED_BLOCK_HEIGHT,
  pattern GL_UNPACK_COMPRESSED_BLOCK_SIZE,
  pattern GL_UNPACK_COMPRESSED_BLOCK_WIDTH
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
