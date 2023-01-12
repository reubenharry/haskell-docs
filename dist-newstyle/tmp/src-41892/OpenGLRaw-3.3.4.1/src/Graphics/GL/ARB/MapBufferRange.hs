{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.MapBufferRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.MapBufferRange (
  -- * Extension Support
  glGetARBMapBufferRange,
  gl_ARB_map_buffer_range,
  -- * Enums
  pattern GL_MAP_FLUSH_EXPLICIT_BIT,
  pattern GL_MAP_INVALIDATE_BUFFER_BIT,
  pattern GL_MAP_INVALIDATE_RANGE_BIT,
  pattern GL_MAP_READ_BIT,
  pattern GL_MAP_UNSYNCHRONIZED_BIT,
  pattern GL_MAP_WRITE_BIT,
  -- * Functions
  glFlushMappedBufferRange,
  glMapBufferRange
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
