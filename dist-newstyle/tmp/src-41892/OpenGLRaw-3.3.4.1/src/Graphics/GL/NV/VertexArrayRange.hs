{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexArrayRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexArrayRange (
  -- * Extension Support
  glGetNVVertexArrayRange,
  gl_NV_vertex_array_range,
  -- * Enums
  pattern GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV,
  pattern GL_VERTEX_ARRAY_RANGE_LENGTH_NV,
  pattern GL_VERTEX_ARRAY_RANGE_NV,
  pattern GL_VERTEX_ARRAY_RANGE_POINTER_NV,
  pattern GL_VERTEX_ARRAY_RANGE_VALID_NV,
  -- * Functions
  glFlushVertexArrayRangeNV,
  glVertexArrayRangeNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
