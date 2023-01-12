{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.IBM.RasterposClip
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.IBM.RasterposClip (
  -- * Extension Support
  glGetIBMRasterposClip,
  gl_IBM_rasterpos_clip,
  -- * Enums
  pattern GL_RASTER_POSITION_UNCLIPPED_IBM
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
