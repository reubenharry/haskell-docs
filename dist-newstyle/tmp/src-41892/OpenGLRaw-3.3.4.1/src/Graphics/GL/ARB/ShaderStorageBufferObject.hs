{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShaderStorageBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShaderStorageBufferObject (
  -- * Extension Support
  glGetARBShaderStorageBufferObject,
  gl_ARB_shader_storage_buffer_object,
  -- * Enums
  pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS,
  pattern GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES,
  pattern GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_SHADER_STORAGE_BLOCK_SIZE,
  pattern GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS,
  pattern GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS,
  pattern GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS,
  pattern GL_SHADER_STORAGE_BARRIER_BIT,
  pattern GL_SHADER_STORAGE_BUFFER,
  pattern GL_SHADER_STORAGE_BUFFER_BINDING,
  pattern GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT,
  pattern GL_SHADER_STORAGE_BUFFER_SIZE,
  pattern GL_SHADER_STORAGE_BUFFER_START,
  -- * Functions
  glShaderStorageBlockBinding
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
