{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.VertexArrayRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.VertexArrayRange (
  -- * Extension Support
  glGetAPPLEVertexArrayRange,
  gl_APPLE_vertex_array_range,
  -- * Enums
  pattern GL_STORAGE_CACHED_APPLE,
  pattern GL_STORAGE_CLIENT_APPLE,
  pattern GL_STORAGE_SHARED_APPLE,
  pattern GL_VERTEX_ARRAY_RANGE_APPLE,
  pattern GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE,
  pattern GL_VERTEX_ARRAY_RANGE_POINTER_APPLE,
  pattern GL_VERTEX_ARRAY_STORAGE_HINT_APPLE,
  -- * Functions
  glFlushVertexArrayRangeAPPLE,
  glVertexArrayParameteriAPPLE,
  glVertexArrayRangeAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
