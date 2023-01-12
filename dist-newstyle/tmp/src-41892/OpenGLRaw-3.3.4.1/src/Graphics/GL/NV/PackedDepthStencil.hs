{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.PackedDepthStencil
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.PackedDepthStencil (
  -- * Extension Support
  glGetNVPackedDepthStencil,
  gl_NV_packed_depth_stencil,
  -- * Enums
  pattern GL_DEPTH_STENCIL_NV,
  pattern GL_UNSIGNED_INT_24_8_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
