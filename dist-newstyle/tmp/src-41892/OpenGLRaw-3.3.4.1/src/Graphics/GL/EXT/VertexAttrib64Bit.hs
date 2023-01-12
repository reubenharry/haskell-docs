{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.VertexAttrib64Bit
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.VertexAttrib64Bit (
  -- * Extension Support
  glGetEXTVertexAttrib64Bit,
  gl_EXT_vertex_attrib_64bit,
  -- * Enums
  pattern GL_DOUBLE,
  pattern GL_DOUBLE_MAT2_EXT,
  pattern GL_DOUBLE_MAT2x3_EXT,
  pattern GL_DOUBLE_MAT2x4_EXT,
  pattern GL_DOUBLE_MAT3_EXT,
  pattern GL_DOUBLE_MAT3x2_EXT,
  pattern GL_DOUBLE_MAT3x4_EXT,
  pattern GL_DOUBLE_MAT4_EXT,
  pattern GL_DOUBLE_MAT4x2_EXT,
  pattern GL_DOUBLE_MAT4x3_EXT,
  pattern GL_DOUBLE_VEC2_EXT,
  pattern GL_DOUBLE_VEC3_EXT,
  pattern GL_DOUBLE_VEC4_EXT,
  -- * Functions
  glGetVertexAttribLdvEXT,
  glVertexAttribL1dEXT,
  glVertexAttribL1dvEXT,
  glVertexAttribL2dEXT,
  glVertexAttribL2dvEXT,
  glVertexAttribL3dEXT,
  glVertexAttribL3dvEXT,
  glVertexAttribL4dEXT,
  glVertexAttribL4dvEXT,
  glVertexAttribLPointerEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
