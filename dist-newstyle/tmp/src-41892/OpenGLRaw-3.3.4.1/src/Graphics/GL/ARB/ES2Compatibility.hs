{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ES2Compatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ES2Compatibility (
  -- * Extension Support
  glGetARBES2Compatibility,
  gl_ARB_ES2_compatibility,
  -- * Enums
  pattern GL_FIXED,
  pattern GL_HIGH_FLOAT,
  pattern GL_HIGH_INT,
  pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT,
  pattern GL_IMPLEMENTATION_COLOR_READ_TYPE,
  pattern GL_LOW_FLOAT,
  pattern GL_LOW_INT,
  pattern GL_MAX_FRAGMENT_UNIFORM_VECTORS,
  pattern GL_MAX_VARYING_VECTORS,
  pattern GL_MAX_VERTEX_UNIFORM_VECTORS,
  pattern GL_MEDIUM_FLOAT,
  pattern GL_MEDIUM_INT,
  pattern GL_NUM_SHADER_BINARY_FORMATS,
  pattern GL_RGB565,
  pattern GL_SHADER_BINARY_FORMATS,
  pattern GL_SHADER_COMPILER,
  -- * Functions
  glClearDepthf,
  glDepthRangef,
  glGetShaderPrecisionFormat,
  glReleaseShaderCompiler,
  glShaderBinary
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
