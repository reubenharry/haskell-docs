{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESA.TileRasterOrder
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESA.TileRasterOrder (
  -- * Extension Support
  glGetMESATileRasterOrder,
  gl_MESA_tile_raster_order,
  -- * Enums
  pattern GL_TILE_RASTER_ORDER_FIXED_MESA,
  pattern GL_TILE_RASTER_ORDER_INCREASING_X_MESA,
  pattern GL_TILE_RASTER_ORDER_INCREASING_Y_MESA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
