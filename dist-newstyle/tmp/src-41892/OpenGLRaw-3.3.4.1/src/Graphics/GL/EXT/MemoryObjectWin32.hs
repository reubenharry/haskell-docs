{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.MemoryObjectWin32
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.MemoryObjectWin32 (
  -- * Extension Support
  glGetEXTMemoryObjectWin32,
  gl_EXT_memory_object_win32,
  -- * Enums
  pattern GL_DEVICE_LUID_EXT,
  pattern GL_DEVICE_NODE_MASK_EXT,
  pattern GL_HANDLE_TYPE_D3D11_IMAGE_EXT,
  pattern GL_HANDLE_TYPE_D3D11_IMAGE_KMT_EXT,
  pattern GL_HANDLE_TYPE_D3D12_RESOURCE_EXT,
  pattern GL_HANDLE_TYPE_D3D12_TILEPOOL_EXT,
  pattern GL_HANDLE_TYPE_OPAQUE_WIN32_EXT,
  pattern GL_HANDLE_TYPE_OPAQUE_WIN32_KMT_EXT,
  pattern GL_LUID_SIZE_EXT,
  -- * Functions
  glImportMemoryWin32HandleEXT,
  glImportMemoryWin32NameEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
