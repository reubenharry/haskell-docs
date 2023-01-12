{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NVX.LinkedGPUMulticast
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NVX.LinkedGPUMulticast (
  -- * Extension Support
  glGetNVXLinkedGPUMulticast,
  gl_NVX_linked_gpu_multicast,
  -- * Enums
  pattern GL_LGPU_SEPARATE_STORAGE_BIT_NVX,
  pattern GL_MAX_LGPU_GPUS_NVX,
  -- * Functions
  glLGPUCopyImageSubDataNVX,
  glLGPUInterlockNVX,
  glLGPUNamedBufferSubDataNVX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
