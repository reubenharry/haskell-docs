{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ComputeShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ComputeShader (
  -- * Extension Support
  glGetARBComputeShader,
  gl_ARB_compute_shader,
  -- * Enums
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER,
  pattern GL_COMPUTE_SHADER,
  pattern GL_COMPUTE_SHADER_BIT,
  pattern GL_COMPUTE_WORK_GROUP_SIZE,
  pattern GL_DISPATCH_INDIRECT_BUFFER,
  pattern GL_DISPATCH_INDIRECT_BUFFER_BINDING,
  pattern GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS,
  pattern GL_MAX_COMPUTE_ATOMIC_COUNTERS,
  pattern GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_COMPUTE_IMAGE_UNIFORMS,
  pattern GL_MAX_COMPUTE_SHARED_MEMORY_SIZE,
  pattern GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS,
  pattern GL_MAX_COMPUTE_UNIFORM_BLOCKS,
  pattern GL_MAX_COMPUTE_UNIFORM_COMPONENTS,
  pattern GL_MAX_COMPUTE_WORK_GROUP_COUNT,
  pattern GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS,
  pattern GL_MAX_COMPUTE_WORK_GROUP_SIZE,
  pattern GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER,
  -- * Functions
  glDispatchCompute,
  glDispatchComputeIndirect
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
