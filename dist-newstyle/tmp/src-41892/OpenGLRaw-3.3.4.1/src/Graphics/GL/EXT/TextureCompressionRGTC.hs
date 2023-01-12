{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureCompressionRGTC
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureCompressionRGTC (
  -- * Extension Support
  glGetEXTTextureCompressionRGTC,
  gl_EXT_texture_compression_rgtc,
  -- * Enums
  pattern GL_COMPRESSED_RED_GREEN_RGTC2_EXT,
  pattern GL_COMPRESSED_RED_RGTC1_EXT,
  pattern GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT,
  pattern GL_COMPRESSED_SIGNED_RED_RGTC1_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
