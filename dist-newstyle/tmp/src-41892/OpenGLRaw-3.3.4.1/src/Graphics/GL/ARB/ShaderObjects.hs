{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShaderObjects
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShaderObjects (
  -- * Extension Support
  glGetARBShaderObjects,
  gl_ARB_shader_objects,
  -- * Enums
  pattern GL_BOOL_ARB,
  pattern GL_BOOL_VEC2_ARB,
  pattern GL_BOOL_VEC3_ARB,
  pattern GL_BOOL_VEC4_ARB,
  pattern GL_FLOAT_MAT2_ARB,
  pattern GL_FLOAT_MAT3_ARB,
  pattern GL_FLOAT_MAT4_ARB,
  pattern GL_FLOAT_VEC2_ARB,
  pattern GL_FLOAT_VEC3_ARB,
  pattern GL_FLOAT_VEC4_ARB,
  pattern GL_INT_VEC2_ARB,
  pattern GL_INT_VEC3_ARB,
  pattern GL_INT_VEC4_ARB,
  pattern GL_OBJECT_ACTIVE_UNIFORMS_ARB,
  pattern GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB,
  pattern GL_OBJECT_ATTACHED_OBJECTS_ARB,
  pattern GL_OBJECT_COMPILE_STATUS_ARB,
  pattern GL_OBJECT_DELETE_STATUS_ARB,
  pattern GL_OBJECT_INFO_LOG_LENGTH_ARB,
  pattern GL_OBJECT_LINK_STATUS_ARB,
  pattern GL_OBJECT_SHADER_SOURCE_LENGTH_ARB,
  pattern GL_OBJECT_SUBTYPE_ARB,
  pattern GL_OBJECT_TYPE_ARB,
  pattern GL_OBJECT_VALIDATE_STATUS_ARB,
  pattern GL_PROGRAM_OBJECT_ARB,
  pattern GL_SAMPLER_1D_ARB,
  pattern GL_SAMPLER_1D_SHADOW_ARB,
  pattern GL_SAMPLER_2D_ARB,
  pattern GL_SAMPLER_2D_RECT_ARB,
  pattern GL_SAMPLER_2D_RECT_SHADOW_ARB,
  pattern GL_SAMPLER_2D_SHADOW_ARB,
  pattern GL_SAMPLER_3D_ARB,
  pattern GL_SAMPLER_CUBE_ARB,
  pattern GL_SHADER_OBJECT_ARB,
  -- * Functions
  glAttachObjectARB,
  glCompileShaderARB,
  glCreateProgramObjectARB,
  glCreateShaderObjectARB,
  glDeleteObjectARB,
  glDetachObjectARB,
  glGetActiveUniformARB,
  glGetAttachedObjectsARB,
  glGetHandleARB,
  glGetInfoLogARB,
  glGetObjectParameterfvARB,
  glGetObjectParameterivARB,
  glGetShaderSourceARB,
  glGetUniformLocationARB,
  glGetUniformfvARB,
  glGetUniformivARB,
  glLinkProgramARB,
  glShaderSourceARB,
  glUniform1fARB,
  glUniform1fvARB,
  glUniform1iARB,
  glUniform1ivARB,
  glUniform2fARB,
  glUniform2fvARB,
  glUniform2iARB,
  glUniform2ivARB,
  glUniform3fARB,
  glUniform3fvARB,
  glUniform3iARB,
  glUniform3ivARB,
  glUniform4fARB,
  glUniform4fvARB,
  glUniform4iARB,
  glUniform4ivARB,
  glUniformMatrix2fvARB,
  glUniformMatrix3fvARB,
  glUniformMatrix4fvARB,
  glUseProgramObjectARB,
  glValidateProgramARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
