{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.VertexArrayObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.VertexArrayObject (
  -- * Extension Support
  glGetAPPLEVertexArrayObject,
  gl_APPLE_vertex_array_object,
  -- * Enums
  pattern GL_VERTEX_ARRAY_BINDING_APPLE,
  -- * Functions
  glBindVertexArrayAPPLE,
  glDeleteVertexArraysAPPLE,
  glGenVertexArraysAPPLE,
  glIsVertexArrayAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
