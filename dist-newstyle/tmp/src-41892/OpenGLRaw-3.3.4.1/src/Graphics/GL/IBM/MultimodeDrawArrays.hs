--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.IBM.MultimodeDrawArrays
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.IBM.MultimodeDrawArrays (
  -- * Extension Support
  glGetIBMMultimodeDrawArrays,
  gl_IBM_multimode_draw_arrays,
  -- * Functions
  glMultiModeDrawArraysIBM,
  glMultiModeDrawElementsIBM
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
