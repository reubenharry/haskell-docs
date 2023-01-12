{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.SemaphoreWin32
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.SemaphoreWin32 (
  -- * Extension Support
  glGetEXTSemaphoreWin32,
  gl_EXT_semaphore_win32,
  -- * Enums
  pattern GL_D3D12_FENCE_VALUE_EXT,
  pattern GL_DEVICE_LUID_EXT,
  pattern GL_DEVICE_NODE_MASK_EXT,
  pattern GL_HANDLE_TYPE_D3D12_FENCE_EXT,
  pattern GL_HANDLE_TYPE_OPAQUE_WIN32_EXT,
  pattern GL_HANDLE_TYPE_OPAQUE_WIN32_KMT_EXT,
  pattern GL_LUID_SIZE_EXT,
  -- * Functions
  glImportSemaphoreWin32HandleEXT,
  glImportSemaphoreWin32NameEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
