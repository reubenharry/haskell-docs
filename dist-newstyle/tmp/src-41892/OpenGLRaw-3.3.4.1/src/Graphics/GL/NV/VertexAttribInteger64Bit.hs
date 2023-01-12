{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexAttribInteger64Bit
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexAttribInteger64Bit (
  -- * Extension Support
  glGetNVVertexAttribInteger64Bit,
  gl_NV_vertex_attrib_integer_64bit,
  -- * Enums
  pattern GL_INT64_NV,
  pattern GL_UNSIGNED_INT64_NV,
  -- * Functions
  glGetVertexAttribLi64vNV,
  glGetVertexAttribLui64vNV,
  glVertexAttribL1i64NV,
  glVertexAttribL1i64vNV,
  glVertexAttribL1ui64NV,
  glVertexAttribL1ui64vNV,
  glVertexAttribL2i64NV,
  glVertexAttribL2i64vNV,
  glVertexAttribL2ui64NV,
  glVertexAttribL2ui64vNV,
  glVertexAttribL3i64NV,
  glVertexAttribL3i64vNV,
  glVertexAttribL3ui64NV,
  glVertexAttribL3ui64vNV,
  glVertexAttribL4i64NV,
  glVertexAttribL4i64vNV,
  glVertexAttribL4ui64NV,
  glVertexAttribL4ui64vNV,
  glVertexAttribLFormatNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
