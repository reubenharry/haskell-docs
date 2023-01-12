{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ConservativeRasterPreSnapTriangles
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ConservativeRasterPreSnapTriangles (
  -- * Extension Support
  glGetNVConservativeRasterPreSnapTriangles,
  gl_NV_conservative_raster_pre_snap_triangles,
  -- * Enums
  pattern GL_CONSERVATIVE_RASTER_MODE_NV,
  pattern GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV,
  pattern GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV,
  -- * Functions
  glConservativeRasterParameteriNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
