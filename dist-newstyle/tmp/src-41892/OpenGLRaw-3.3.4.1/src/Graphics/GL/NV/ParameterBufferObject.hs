{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ParameterBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ParameterBufferObject (
  -- * Extension Support
  glGetNVParameterBufferObject,
  gl_NV_parameter_buffer_object,
  -- * Enums
  pattern GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV,
  pattern GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV,
  pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV,
  pattern GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV,
  pattern GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV,
  -- * Functions
  glProgramBufferParametersIivNV,
  glProgramBufferParametersIuivNV,
  glProgramBufferParametersfvNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
