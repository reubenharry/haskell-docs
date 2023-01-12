{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.GPUShader4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.GPUShader4 (
  -- * Extension Support
  glGetEXTGPUShader4,
  gl_EXT_gpu_shader4,
  -- * Enums
  pattern GL_INT_SAMPLER_1D_ARRAY_EXT,
  pattern GL_INT_SAMPLER_1D_EXT,
  pattern GL_INT_SAMPLER_2D_ARRAY_EXT,
  pattern GL_INT_SAMPLER_2D_EXT,
  pattern GL_INT_SAMPLER_2D_RECT_EXT,
  pattern GL_INT_SAMPLER_3D_EXT,
  pattern GL_INT_SAMPLER_BUFFER_EXT,
  pattern GL_INT_SAMPLER_CUBE_EXT,
  pattern GL_MAX_PROGRAM_TEXEL_OFFSET_EXT,
  pattern GL_MIN_PROGRAM_TEXEL_OFFSET_EXT,
  pattern GL_SAMPLER_1D_ARRAY_EXT,
  pattern GL_SAMPLER_1D_ARRAY_SHADOW_EXT,
  pattern GL_SAMPLER_2D_ARRAY_EXT,
  pattern GL_SAMPLER_2D_ARRAY_SHADOW_EXT,
  pattern GL_SAMPLER_BUFFER_EXT,
  pattern GL_SAMPLER_CUBE_SHADOW_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_1D_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_2D_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_3D_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT,
  pattern GL_UNSIGNED_INT_SAMPLER_CUBE_EXT,
  pattern GL_UNSIGNED_INT_VEC2_EXT,
  pattern GL_UNSIGNED_INT_VEC3_EXT,
  pattern GL_UNSIGNED_INT_VEC4_EXT,
  pattern GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT,
  -- * Functions
  glBindFragDataLocationEXT,
  glGetFragDataLocationEXT,
  glGetUniformuivEXT,
  glUniform1uiEXT,
  glUniform1uivEXT,
  glUniform2uiEXT,
  glUniform2uivEXT,
  glUniform3uiEXT,
  glUniform3uivEXT,
  glUniform4uiEXT,
  glUniform4uivEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
