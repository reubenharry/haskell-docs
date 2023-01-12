{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexBufferObject (
  -- * Extension Support
  glGetARBVertexBufferObject,
  gl_ARB_vertex_buffer_object,
  -- * Enums
  pattern GL_ARRAY_BUFFER_ARB,
  pattern GL_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_BUFFER_ACCESS_ARB,
  pattern GL_BUFFER_MAPPED_ARB,
  pattern GL_BUFFER_MAP_POINTER_ARB,
  pattern GL_BUFFER_SIZE_ARB,
  pattern GL_BUFFER_USAGE_ARB,
  pattern GL_COLOR_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_DYNAMIC_COPY_ARB,
  pattern GL_DYNAMIC_DRAW_ARB,
  pattern GL_DYNAMIC_READ_ARB,
  pattern GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_ELEMENT_ARRAY_BUFFER_ARB,
  pattern GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_INDEX_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_NORMAL_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_READ_ONLY_ARB,
  pattern GL_READ_WRITE_ARB,
  pattern GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_STATIC_COPY_ARB,
  pattern GL_STATIC_DRAW_ARB,
  pattern GL_STATIC_READ_ARB,
  pattern GL_STREAM_COPY_ARB,
  pattern GL_STREAM_DRAW_ARB,
  pattern GL_STREAM_READ_ARB,
  pattern GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_VERTEX_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB,
  pattern GL_WRITE_ONLY_ARB,
  -- * Functions
  glBindBufferARB,
  glBufferDataARB,
  glBufferSubDataARB,
  glDeleteBuffersARB,
  glGenBuffersARB,
  glGetBufferParameterivARB,
  glGetBufferPointervARB,
  glGetBufferSubDataARB,
  glIsBufferARB,
  glMapBufferARB,
  glUnmapBufferARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
