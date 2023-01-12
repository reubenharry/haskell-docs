{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NVX.GPUMulticast2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NVX.GPUMulticast2 (
  -- * Extension Support
  glGetNVXGPUMulticast2,
  gl_NVX_gpu_multicast2,
  -- * Enums
  pattern GL_UPLOAD_GPU_MASK_NVX,
  -- * Functions
  glAsyncCopyBufferSubDataNVX,
  glAsyncCopyImageSubDataNVX,
  glMulticastScissorArrayvNVX,
  glMulticastViewportArrayvNVX,
  glMulticastViewportPositionWScaleNVX,
  glUploadGpuMaskNVX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
