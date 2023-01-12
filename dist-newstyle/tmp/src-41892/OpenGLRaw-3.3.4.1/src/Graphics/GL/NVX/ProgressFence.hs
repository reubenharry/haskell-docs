--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NVX.ProgressFence
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NVX.ProgressFence (
  -- * Extension Support
  glGetNVXProgressFence,
  gl_NVX_progress_fence,
  -- * Functions
  glClientWaitSemaphoreui64NVX,
  glCreateProgressFenceNVX,
  glSignalSemaphoreui64NVX,
  glWaitSemaphoreui64NVX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
