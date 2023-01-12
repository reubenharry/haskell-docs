{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ConservativeRasterDilate
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ConservativeRasterDilate (
  -- * Extension Support
  glGetNVConservativeRasterDilate,
  gl_NV_conservative_raster_dilate,
  -- * Enums
  pattern GL_CONSERVATIVE_RASTER_DILATE_GRANULARITY_NV,
  pattern GL_CONSERVATIVE_RASTER_DILATE_NV,
  pattern GL_CONSERVATIVE_RASTER_DILATE_RANGE_NV,
  -- * Functions
  glConservativeRasterParameterfNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
