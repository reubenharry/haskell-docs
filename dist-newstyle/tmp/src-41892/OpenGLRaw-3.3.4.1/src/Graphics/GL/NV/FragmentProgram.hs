{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FragmentProgram
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FragmentProgram (
  -- * Extension Support
  glGetNVFragmentProgram,
  gl_NV_fragment_program,
  -- * Enums
  pattern GL_FRAGMENT_PROGRAM_BINDING_NV,
  pattern GL_FRAGMENT_PROGRAM_NV,
  pattern GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV,
  pattern GL_MAX_TEXTURE_COORDS_NV,
  pattern GL_MAX_TEXTURE_IMAGE_UNITS_NV,
  pattern GL_PROGRAM_ERROR_STRING_NV,
  -- * Functions
  glGetProgramNamedParameterdvNV,
  glGetProgramNamedParameterfvNV,
  glProgramNamedParameter4dNV,
  glProgramNamedParameter4dvNV,
  glProgramNamedParameter4fNV,
  glProgramNamedParameter4fvNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
