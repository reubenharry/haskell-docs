{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.ParallelArrays
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.ParallelArrays (
  -- * Extension Support
  glGetINTELParallelArrays,
  gl_INTEL_parallel_arrays,
  -- * Enums
  pattern GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL,
  pattern GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL,
  pattern GL_PARALLEL_ARRAYS_INTEL,
  pattern GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL,
  pattern GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL,
  -- * Functions
  glColorPointervINTEL,
  glNormalPointervINTEL,
  glTexCoordPointervINTEL,
  glVertexPointervINTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
