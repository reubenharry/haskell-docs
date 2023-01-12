{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.GPUShaderInt64
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.GPUShaderInt64 (
  -- * Extension Support
  glGetAMDGPUShaderInt64,
  gl_AMD_gpu_shader_int64,
  -- * Enums
  pattern GL_FLOAT16_NV,
  pattern GL_FLOAT16_VEC2_NV,
  pattern GL_FLOAT16_VEC3_NV,
  pattern GL_FLOAT16_VEC4_NV,
  pattern GL_INT16_NV,
  pattern GL_INT16_VEC2_NV,
  pattern GL_INT16_VEC3_NV,
  pattern GL_INT16_VEC4_NV,
  pattern GL_INT64_NV,
  pattern GL_INT64_VEC2_NV,
  pattern GL_INT64_VEC3_NV,
  pattern GL_INT64_VEC4_NV,
  pattern GL_INT8_NV,
  pattern GL_INT8_VEC2_NV,
  pattern GL_INT8_VEC3_NV,
  pattern GL_INT8_VEC4_NV,
  pattern GL_UNSIGNED_INT16_NV,
  pattern GL_UNSIGNED_INT16_VEC2_NV,
  pattern GL_UNSIGNED_INT16_VEC3_NV,
  pattern GL_UNSIGNED_INT16_VEC4_NV,
  pattern GL_UNSIGNED_INT64_NV,
  pattern GL_UNSIGNED_INT64_VEC2_NV,
  pattern GL_UNSIGNED_INT64_VEC3_NV,
  pattern GL_UNSIGNED_INT64_VEC4_NV,
  pattern GL_UNSIGNED_INT8_NV,
  pattern GL_UNSIGNED_INT8_VEC2_NV,
  pattern GL_UNSIGNED_INT8_VEC3_NV,
  pattern GL_UNSIGNED_INT8_VEC4_NV,
  -- * Functions
  glGetUniformi64vNV,
  glGetUniformui64vNV,
  glProgramUniform1i64NV,
  glProgramUniform1i64vNV,
  glProgramUniform1ui64NV,
  glProgramUniform1ui64vNV,
  glProgramUniform2i64NV,
  glProgramUniform2i64vNV,
  glProgramUniform2ui64NV,
  glProgramUniform2ui64vNV,
  glProgramUniform3i64NV,
  glProgramUniform3i64vNV,
  glProgramUniform3ui64NV,
  glProgramUniform3ui64vNV,
  glProgramUniform4i64NV,
  glProgramUniform4i64vNV,
  glProgramUniform4ui64NV,
  glProgramUniform4ui64vNV,
  glUniform1i64NV,
  glUniform1i64vNV,
  glUniform1ui64NV,
  glUniform1ui64vNV,
  glUniform2i64NV,
  glUniform2i64vNV,
  glUniform2ui64NV,
  glUniform2ui64vNV,
  glUniform3i64NV,
  glUniform3i64vNV,
  glUniform3ui64NV,
  glUniform3ui64vNV,
  glUniform4i64NV,
  glUniform4i64vNV,
  glUniform4ui64NV,
  glUniform4ui64vNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
