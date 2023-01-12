{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGI.ColorMatrix
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGI.ColorMatrix (
  -- * Extension Support
  glGetSGIColorMatrix,
  gl_SGI_color_matrix,
  -- * Enums
  pattern GL_COLOR_MATRIX_SGI,
  pattern GL_COLOR_MATRIX_STACK_DEPTH_SGI,
  pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI,
  pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI,
  pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI,
  pattern GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI,
  pattern GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI,
  pattern GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI,
  pattern GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI,
  pattern GL_POST_COLOR_MATRIX_RED_BIAS_SGI,
  pattern GL_POST_COLOR_MATRIX_RED_SCALE_SGI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
