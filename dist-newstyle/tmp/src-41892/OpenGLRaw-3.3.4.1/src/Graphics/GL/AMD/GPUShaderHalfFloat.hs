{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.GPUShaderHalfFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.GPUShaderHalfFloat (
  -- * Extension Support
  glGetAMDGPUShaderHalfFloat,
  gl_AMD_gpu_shader_half_float,
  -- * Enums
  pattern GL_FLOAT16_MAT2_AMD,
  pattern GL_FLOAT16_MAT2x3_AMD,
  pattern GL_FLOAT16_MAT2x4_AMD,
  pattern GL_FLOAT16_MAT3_AMD,
  pattern GL_FLOAT16_MAT3x2_AMD,
  pattern GL_FLOAT16_MAT3x4_AMD,
  pattern GL_FLOAT16_MAT4_AMD,
  pattern GL_FLOAT16_MAT4x2_AMD,
  pattern GL_FLOAT16_MAT4x3_AMD,
  pattern GL_FLOAT16_NV,
  pattern GL_FLOAT16_VEC2_NV,
  pattern GL_FLOAT16_VEC3_NV,
  pattern GL_FLOAT16_VEC4_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
