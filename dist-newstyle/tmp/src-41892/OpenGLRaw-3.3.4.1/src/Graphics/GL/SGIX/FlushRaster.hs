--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.FlushRaster
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.FlushRaster (
  -- * Extension Support
  glGetSGIXFlushRaster,
  gl_SGIX_flush_raster,
  -- * Functions
  glFlushRasterSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
