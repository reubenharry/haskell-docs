{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.GPUProgram4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.GPUProgram4 (
  -- * Extension Support
  glGetNVGPUProgram4,
  gl_NV_gpu_program4,
  -- * Enums
  pattern GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV,
  pattern GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV,
  pattern GL_MAX_PROGRAM_GENERIC_RESULTS_NV,
  pattern GL_MAX_PROGRAM_RESULT_COMPONENTS_NV,
  pattern GL_MAX_PROGRAM_TEXEL_OFFSET_NV,
  pattern GL_MIN_PROGRAM_TEXEL_OFFSET_NV,
  pattern GL_PROGRAM_ATTRIB_COMPONENTS_NV,
  pattern GL_PROGRAM_RESULT_COMPONENTS_NV,
  -- * Functions
  glGetProgramEnvParameterIivNV,
  glGetProgramEnvParameterIuivNV,
  glGetProgramLocalParameterIivNV,
  glGetProgramLocalParameterIuivNV,
  glProgramEnvParameterI4iNV,
  glProgramEnvParameterI4ivNV,
  glProgramEnvParameterI4uiNV,
  glProgramEnvParameterI4uivNV,
  glProgramEnvParametersI4ivNV,
  glProgramEnvParametersI4uivNV,
  glProgramLocalParameterI4iNV,
  glProgramLocalParameterI4ivNV,
  glProgramLocalParameterI4uiNV,
  glProgramLocalParameterI4uivNV,
  glProgramLocalParametersI4ivNV,
  glProgramLocalParametersI4uivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
