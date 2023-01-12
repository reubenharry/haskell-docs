{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GPUShaderFP64
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GPUShaderFP64 (
  -- * Extension Support
  glGetARBGPUShaderFP64,
  gl_ARB_gpu_shader_fp64,
  -- * Enums
  pattern GL_DOUBLE,
  pattern GL_DOUBLE_MAT2,
  pattern GL_DOUBLE_MAT2x3,
  pattern GL_DOUBLE_MAT2x4,
  pattern GL_DOUBLE_MAT3,
  pattern GL_DOUBLE_MAT3x2,
  pattern GL_DOUBLE_MAT3x4,
  pattern GL_DOUBLE_MAT4,
  pattern GL_DOUBLE_MAT4x2,
  pattern GL_DOUBLE_MAT4x3,
  pattern GL_DOUBLE_VEC2,
  pattern GL_DOUBLE_VEC3,
  pattern GL_DOUBLE_VEC4,
  -- * Functions
  glGetUniformdv,
  glUniform1d,
  glUniform1dv,
  glUniform2d,
  glUniform2dv,
  glUniform3d,
  glUniform3dv,
  glUniform4d,
  glUniform4dv,
  glUniformMatrix2dv,
  glUniformMatrix2x3dv,
  glUniformMatrix2x4dv,
  glUniformMatrix3dv,
  glUniformMatrix3x2dv,
  glUniformMatrix3x4dv,
  glUniformMatrix4dv,
  glUniformMatrix4x2dv,
  glUniformMatrix4x3dv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
