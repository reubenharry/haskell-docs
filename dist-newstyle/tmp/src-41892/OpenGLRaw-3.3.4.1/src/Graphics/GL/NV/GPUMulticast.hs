{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.GPUMulticast
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.GPUMulticast (
  -- * Extension Support
  glGetNVGPUMulticast,
  gl_NV_gpu_multicast,
  -- * Enums
  pattern GL_MULTICAST_GPUS_NV,
  pattern GL_MULTICAST_PROGRAMMABLE_SAMPLE_LOCATION_NV,
  pattern GL_PER_GPU_STORAGE_BIT_NV,
  pattern GL_PER_GPU_STORAGE_NV,
  pattern GL_RENDER_GPU_MASK_NV,
  -- * Functions
  glMulticastBarrierNV,
  glMulticastBlitFramebufferNV,
  glMulticastBufferSubDataNV,
  glMulticastCopyBufferSubDataNV,
  glMulticastCopyImageSubDataNV,
  glMulticastFramebufferSampleLocationsfvNV,
  glMulticastGetQueryObjecti64vNV,
  glMulticastGetQueryObjectivNV,
  glMulticastGetQueryObjectui64vNV,
  glMulticastGetQueryObjectuivNV,
  glMulticastWaitSyncNV,
  glRenderGpuMaskNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
