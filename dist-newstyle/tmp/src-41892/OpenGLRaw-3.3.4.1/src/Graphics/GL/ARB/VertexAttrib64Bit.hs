{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexAttrib64Bit
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexAttrib64Bit (
  -- * Extension Support
  glGetARBVertexAttrib64Bit,
  gl_ARB_vertex_attrib_64bit,
  -- * Enums
  pattern GL_DOUBLE_MAT2,
  pattern GL_DOUBLE_MAT2x3,
  pattern GL_DOUBLE_MAT2x4,
  pattern GL_DOUBLE_MAT3,
  pattern GL_DOUBLE_MAT3x2,
  pattern GL_DOUBLE_MAT3x4,
  pattern GL_DOUBLE_MAT4,
  pattern GL_DOUBLE_MAT4x2,
  pattern GL_DOUBLE_MAT4x3,
  pattern GL_DOUBLE_VEC2,
  pattern GL_DOUBLE_VEC3,
  pattern GL_DOUBLE_VEC4,
  pattern GL_RGB32I,
  -- * Functions
  glGetVertexAttribLdv,
  glVertexAttribL1d,
  glVertexAttribL1dv,
  glVertexAttribL2d,
  glVertexAttribL2dv,
  glVertexAttribL3d,
  glVertexAttribL3dv,
  glVertexAttribL4d,
  glVertexAttribL4dv,
  glVertexAttribLPointer
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
