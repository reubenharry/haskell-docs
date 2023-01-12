{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TransposeMatrix
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TransposeMatrix (
  -- * Extension Support
  glGetARBTransposeMatrix,
  gl_ARB_transpose_matrix,
  -- * Enums
  pattern GL_TRANSPOSE_COLOR_MATRIX_ARB,
  pattern GL_TRANSPOSE_MODELVIEW_MATRIX_ARB,
  pattern GL_TRANSPOSE_PROJECTION_MATRIX_ARB,
  pattern GL_TRANSPOSE_TEXTURE_MATRIX_ARB,
  -- * Functions
  glLoadTransposeMatrixdARB,
  glLoadTransposeMatrixfARB,
  glMultTransposeMatrixdARB,
  glMultTransposeMatrixfARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
