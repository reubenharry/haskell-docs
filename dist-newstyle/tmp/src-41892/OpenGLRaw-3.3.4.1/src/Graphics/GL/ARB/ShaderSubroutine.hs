{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShaderSubroutine
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShaderSubroutine (
  -- * Extension Support
  glGetARBShaderSubroutine,
  gl_ARB_shader_subroutine,
  -- * Enums
  pattern GL_ACTIVE_SUBROUTINES,
  pattern GL_ACTIVE_SUBROUTINE_MAX_LENGTH,
  pattern GL_ACTIVE_SUBROUTINE_UNIFORMS,
  pattern GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS,
  pattern GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH,
  pattern GL_COMPATIBLE_SUBROUTINES,
  pattern GL_MAX_SUBROUTINES,
  pattern GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS,
  pattern GL_NUM_COMPATIBLE_SUBROUTINES,
  pattern GL_UNIFORM_NAME_LENGTH,
  pattern GL_UNIFORM_SIZE,
  -- * Functions
  glGetActiveSubroutineName,
  glGetActiveSubroutineUniformName,
  glGetActiveSubroutineUniformiv,
  glGetProgramStageiv,
  glGetSubroutineIndex,
  glGetSubroutineUniformLocation,
  glGetUniformSubroutineuiv,
  glUniformSubroutinesuiv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
