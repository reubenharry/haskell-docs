{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureCompressionRGTC
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureCompressionRGTC (
  -- * Extension Support
  glGetARBTextureCompressionRGTC,
  gl_ARB_texture_compression_rgtc,
  -- * Enums
  pattern GL_COMPRESSED_RED_RGTC1,
  pattern GL_COMPRESSED_RG_RGTC2,
  pattern GL_COMPRESSED_SIGNED_RED_RGTC1,
  pattern GL_COMPRESSED_SIGNED_RG_RGTC2
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
