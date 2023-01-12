{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NVX.GPUMemoryInfo
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NVX.GPUMemoryInfo (
  -- * Extension Support
  glGetNVXGPUMemoryInfo,
  gl_NVX_gpu_memory_info,
  -- * Enums
  pattern GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX,
  pattern GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX,
  pattern GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX,
  pattern GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX,
  pattern GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
