{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ThreeDFX.TextureCompressionFXT1
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ThreeDFX.TextureCompressionFXT1 (
  -- * Extension Support
  glGetThreeDFXTextureCompressionFXT1,
  gl_3DFX_texture_compression_FXT1,
  -- * Enums
  pattern GL_COMPRESSED_RGBA_FXT1_3DFX,
  pattern GL_COMPRESSED_RGB_FXT1_3DFX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
