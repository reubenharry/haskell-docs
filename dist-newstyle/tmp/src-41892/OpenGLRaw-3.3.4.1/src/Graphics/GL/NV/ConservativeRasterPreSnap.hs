{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ConservativeRasterPreSnap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ConservativeRasterPreSnap (
  -- * Extension Support
  glGetNVConservativeRasterPreSnap,
  gl_NV_conservative_raster_pre_snap,
  -- * Enums
  pattern GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
