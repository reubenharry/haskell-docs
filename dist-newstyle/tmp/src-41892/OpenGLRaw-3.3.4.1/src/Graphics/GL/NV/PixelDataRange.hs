{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.PixelDataRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.PixelDataRange (
  -- * Extension Support
  glGetNVPixelDataRange,
  gl_NV_pixel_data_range,
  -- * Enums
  pattern GL_READ_PIXEL_DATA_RANGE_LENGTH_NV,
  pattern GL_READ_PIXEL_DATA_RANGE_NV,
  pattern GL_READ_PIXEL_DATA_RANGE_POINTER_NV,
  pattern GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV,
  pattern GL_WRITE_PIXEL_DATA_RANGE_NV,
  pattern GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV,
  -- * Functions
  glFlushPixelDataRangeNV,
  glPixelDataRangeNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
