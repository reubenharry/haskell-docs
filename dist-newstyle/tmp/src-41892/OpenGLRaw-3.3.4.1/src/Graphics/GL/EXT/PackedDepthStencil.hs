{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PackedDepthStencil
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PackedDepthStencil (
  -- * Extension Support
  glGetEXTPackedDepthStencil,
  gl_EXT_packed_depth_stencil,
  -- * Enums
  pattern GL_DEPTH24_STENCIL8_EXT,
  pattern GL_DEPTH_STENCIL_EXT,
  pattern GL_TEXTURE_STENCIL_SIZE_EXT,
  pattern GL_UNSIGNED_INT_24_8_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
