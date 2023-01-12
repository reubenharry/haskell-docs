{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.BufferStorage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.BufferStorage (
  -- * Extension Support
  glGetARBBufferStorage,
  gl_ARB_buffer_storage,
  -- * Enums
  pattern GL_BUFFER_IMMUTABLE_STORAGE,
  pattern GL_BUFFER_STORAGE_FLAGS,
  pattern GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT,
  pattern GL_CLIENT_STORAGE_BIT,
  pattern GL_DYNAMIC_STORAGE_BIT,
  pattern GL_MAP_COHERENT_BIT,
  pattern GL_MAP_PERSISTENT_BIT,
  pattern GL_MAP_READ_BIT,
  pattern GL_MAP_WRITE_BIT,
  -- * Functions
  glBufferStorage
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
