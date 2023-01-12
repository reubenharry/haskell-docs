{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.GPUProgram5
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.GPUProgram5 (
  -- * Extension Support
  glGetNVGPUProgram5,
  gl_NV_gpu_program5,
  -- * Enums
  pattern GL_FRAGMENT_PROGRAM_INTERPOLATION_OFFSET_BITS_NV,
  pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_NV,
  pattern GL_MAX_GEOMETRY_PROGRAM_INVOCATIONS_NV,
  pattern GL_MAX_PROGRAM_SUBROUTINE_NUM_NV,
  pattern GL_MAX_PROGRAM_SUBROUTINE_PARAMETERS_NV,
  pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_NV,
  pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_NV,
  pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_NV,
  -- * Functions
  glGetProgramSubroutineParameteruivNV,
  glProgramSubroutineParametersuivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
