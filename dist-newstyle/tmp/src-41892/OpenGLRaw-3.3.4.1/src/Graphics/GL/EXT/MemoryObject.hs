{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.MemoryObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.MemoryObject (
  -- * Extension Support
  glGetEXTMemoryObject,
  gl_EXT_memory_object,
  -- * Enums
  pattern GL_DEDICATED_MEMORY_OBJECT_EXT,
  pattern GL_DEVICE_UUID_EXT,
  pattern GL_DRIVER_UUID_EXT,
  pattern GL_LINEAR_TILING_EXT,
  pattern GL_NUM_DEVICE_UUIDS_EXT,
  pattern GL_NUM_TILING_TYPES_EXT,
  pattern GL_OPTIMAL_TILING_EXT,
  pattern GL_PROTECTED_MEMORY_OBJECT_EXT,
  pattern GL_TEXTURE_TILING_EXT,
  pattern GL_TILING_TYPES_EXT,
  pattern GL_UUID_SIZE_EXT,
  -- * Functions
  glBufferStorageMemEXT,
  glCreateMemoryObjectsEXT,
  glDeleteMemoryObjectsEXT,
  glGetMemoryObjectParameterivEXT,
  glGetUnsignedBytei_vEXT,
  glGetUnsignedBytevEXT,
  glIsMemoryObjectEXT,
  glMemoryObjectParameterivEXT,
  glNamedBufferStorageMemEXT,
  glTexStorageMem1DEXT,
  glTexStorageMem2DEXT,
  glTexStorageMem2DMultisampleEXT,
  glTexStorageMem3DEXT,
  glTexStorageMem3DMultisampleEXT,
  glTextureStorageMem1DEXT,
  glTextureStorageMem2DEXT,
  glTextureStorageMem2DMultisampleEXT,
  glTextureStorageMem3DEXT,
  glTextureStorageMem3DMultisampleEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
