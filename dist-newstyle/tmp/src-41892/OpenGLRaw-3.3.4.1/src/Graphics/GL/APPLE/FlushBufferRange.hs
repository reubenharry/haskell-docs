{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.FlushBufferRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.FlushBufferRange (
  -- * Extension Support
  glGetAPPLEFlushBufferRange,
  gl_APPLE_flush_buffer_range,
  -- * Enums
  pattern GL_BUFFER_FLUSHING_UNMAP_APPLE,
  pattern GL_BUFFER_SERIALIZED_MODIFY_APPLE,
  -- * Functions
  glBufferParameteriAPPLE,
  glFlushMappedBufferRangeAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
