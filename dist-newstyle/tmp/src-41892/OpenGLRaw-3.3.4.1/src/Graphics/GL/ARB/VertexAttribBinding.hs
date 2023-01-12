{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexAttribBinding
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexAttribBinding (
  -- * Extension Support
  glGetARBVertexAttribBinding,
  gl_ARB_vertex_attrib_binding,
  -- * Enums
  pattern GL_MAX_VERTEX_ATTRIB_BINDINGS,
  pattern GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET,
  pattern GL_VERTEX_ATTRIB_BINDING,
  pattern GL_VERTEX_ATTRIB_RELATIVE_OFFSET,
  pattern GL_VERTEX_BINDING_DIVISOR,
  pattern GL_VERTEX_BINDING_OFFSET,
  pattern GL_VERTEX_BINDING_STRIDE,
  -- * Functions
  glBindVertexBuffer,
  glVertexAttribBinding,
  glVertexAttribFormat,
  glVertexAttribIFormat,
  glVertexAttribLFormat,
  glVertexBindingDivisor
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
