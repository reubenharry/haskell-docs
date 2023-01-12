{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ShaderBufferLoad
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ShaderBufferLoad (
  -- * Extension Support
  glGetNVShaderBufferLoad,
  gl_NV_shader_buffer_load,
  -- * Enums
  pattern GL_BUFFER_GPU_ADDRESS_NV,
  pattern GL_GPU_ADDRESS_NV,
  pattern GL_MAX_SHADER_BUFFER_ADDRESS_NV,
  -- * Functions
  glGetBufferParameterui64vNV,
  glGetIntegerui64vNV,
  glGetNamedBufferParameterui64vNV,
  glGetUniformui64vNV,
  glIsBufferResidentNV,
  glIsNamedBufferResidentNV,
  glMakeBufferNonResidentNV,
  glMakeBufferResidentNV,
  glMakeNamedBufferNonResidentNV,
  glMakeNamedBufferResidentNV,
  glProgramUniformui64NV,
  glProgramUniformui64vNV,
  glUniformui64NV,
  glUniformui64vNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
