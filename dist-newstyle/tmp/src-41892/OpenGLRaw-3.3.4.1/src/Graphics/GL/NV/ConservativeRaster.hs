{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ConservativeRaster
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ConservativeRaster (
  -- * Extension Support
  glGetNVConservativeRaster,
  gl_NV_conservative_raster,
  -- * Enums
  pattern GL_CONSERVATIVE_RASTERIZATION_NV,
  pattern GL_MAX_SUBPIXEL_PRECISION_BIAS_BITS_NV,
  pattern GL_SUBPIXEL_PRECISION_BIAS_X_BITS_NV,
  pattern GL_SUBPIXEL_PRECISION_BIAS_Y_BITS_NV,
  -- * Functions
  glSubpixelPrecisionBiasNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
