--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESA.ResizeBuffers
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESA.ResizeBuffers (
  -- * Extension Support
  glGetMESAResizeBuffers,
  gl_MESA_resize_buffers,
  -- * Functions
  glResizeBuffersMESA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
