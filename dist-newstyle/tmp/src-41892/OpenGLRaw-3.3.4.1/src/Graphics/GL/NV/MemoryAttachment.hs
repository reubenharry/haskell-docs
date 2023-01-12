{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.MemoryAttachment
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.MemoryAttachment (
  -- * Extension Support
  glGetNVMemoryAttachment,
  gl_NV_memory_attachment,
  -- * Enums
  pattern GL_ATTACHED_MEMORY_OBJECT_NV,
  pattern GL_ATTACHED_MEMORY_OFFSET_NV,
  pattern GL_DETACHED_BUFFERS_NV,
  pattern GL_DETACHED_MEMORY_INCARNATION_NV,
  pattern GL_DETACHED_TEXTURES_NV,
  pattern GL_MAX_DETACHED_BUFFERS_NV,
  pattern GL_MAX_DETACHED_TEXTURES_NV,
  pattern GL_MEMORY_ATTACHABLE_ALIGNMENT_NV,
  pattern GL_MEMORY_ATTACHABLE_NV,
  pattern GL_MEMORY_ATTACHABLE_SIZE_NV,
  -- * Functions
  glBufferAttachMemoryNV,
  glGetMemoryObjectDetachedResourcesuivNV,
  glNamedBufferAttachMemoryNV,
  glResetMemoryObjectParameterNV,
  glTexAttachMemoryNV,
  glTextureAttachMemoryNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
