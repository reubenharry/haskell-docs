{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.MemoryObjectFd
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.MemoryObjectFd (
  -- * Extension Support
  glGetEXTMemoryObjectFd,
  gl_EXT_memory_object_fd,
  -- * Enums
  pattern GL_HANDLE_TYPE_OPAQUE_FD_EXT,
  -- * Functions
  glImportMemoryFdEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
