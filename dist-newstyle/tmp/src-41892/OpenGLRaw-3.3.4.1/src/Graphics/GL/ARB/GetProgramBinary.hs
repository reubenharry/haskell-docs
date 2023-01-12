{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GetProgramBinary
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GetProgramBinary (
  -- * Extension Support
  glGetARBGetProgramBinary,
  gl_ARB_get_program_binary,
  -- * Enums
  pattern GL_NUM_PROGRAM_BINARY_FORMATS,
  pattern GL_PROGRAM_BINARY_FORMATS,
  pattern GL_PROGRAM_BINARY_LENGTH,
  pattern GL_PROGRAM_BINARY_RETRIEVABLE_HINT,
  -- * Functions
  glGetProgramBinary,
  glProgramBinary,
  glProgramParameteri
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
