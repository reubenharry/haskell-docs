{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexArrayObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexArrayObject (
  -- * Extension Support
  glGetARBVertexArrayObject,
  gl_ARB_vertex_array_object,
  -- * Enums
  pattern GL_VERTEX_ARRAY_BINDING,
  -- * Functions
  glBindVertexArray,
  glDeleteVertexArrays,
  glGenVertexArrays,
  glIsVertexArray
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
