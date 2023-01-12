{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.CopyBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.CopyBuffer (
  -- * Extension Support
  glGetARBCopyBuffer,
  gl_ARB_copy_buffer,
  -- * Enums
  pattern GL_COPY_READ_BUFFER,
  pattern GL_COPY_WRITE_BUFFER,
  -- * Functions
  glCopyBufferSubData
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
