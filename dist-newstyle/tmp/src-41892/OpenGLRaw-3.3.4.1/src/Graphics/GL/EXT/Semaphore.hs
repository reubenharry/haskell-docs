{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.Semaphore
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.Semaphore (
  -- * Extension Support
  glGetEXTSemaphore,
  gl_EXT_semaphore,
  -- * Enums
  pattern GL_DEVICE_UUID_EXT,
  pattern GL_DRIVER_UUID_EXT,
  pattern GL_LAYOUT_COLOR_ATTACHMENT_EXT,
  pattern GL_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_EXT,
  pattern GL_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_EXT,
  pattern GL_LAYOUT_DEPTH_STENCIL_ATTACHMENT_EXT,
  pattern GL_LAYOUT_DEPTH_STENCIL_READ_ONLY_EXT,
  pattern GL_LAYOUT_GENERAL_EXT,
  pattern GL_LAYOUT_SHADER_READ_ONLY_EXT,
  pattern GL_LAYOUT_TRANSFER_DST_EXT,
  pattern GL_LAYOUT_TRANSFER_SRC_EXT,
  pattern GL_NUM_DEVICE_UUIDS_EXT,
  pattern GL_UUID_SIZE_EXT,
  -- * Functions
  glDeleteSemaphoresEXT,
  glGenSemaphoresEXT,
  glGetSemaphoreParameterui64vEXT,
  glGetUnsignedBytei_vEXT,
  glGetUnsignedBytevEXT,
  glIsSemaphoreEXT,
  glSemaphoreParameterui64vEXT,
  glSignalSemaphoreEXT,
  glWaitSemaphoreEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
