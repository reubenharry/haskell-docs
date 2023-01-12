{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.MatrixPalette
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.MatrixPalette (
  -- * Extension Support
  glGetARBMatrixPalette,
  gl_ARB_matrix_palette,
  -- * Enums
  pattern GL_CURRENT_MATRIX_INDEX_ARB,
  pattern GL_CURRENT_PALETTE_MATRIX_ARB,
  pattern GL_MATRIX_INDEX_ARRAY_ARB,
  pattern GL_MATRIX_INDEX_ARRAY_POINTER_ARB,
  pattern GL_MATRIX_INDEX_ARRAY_SIZE_ARB,
  pattern GL_MATRIX_INDEX_ARRAY_STRIDE_ARB,
  pattern GL_MATRIX_INDEX_ARRAY_TYPE_ARB,
  pattern GL_MATRIX_PALETTE_ARB,
  pattern GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB,
  pattern GL_MAX_PALETTE_MATRICES_ARB,
  -- * Functions
  glCurrentPaletteMatrixARB,
  glMatrixIndexPointerARB,
  glMatrixIndexubvARB,
  glMatrixIndexuivARB,
  glMatrixIndexusvARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
