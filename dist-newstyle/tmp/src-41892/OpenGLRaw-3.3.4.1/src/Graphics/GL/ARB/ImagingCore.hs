{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ImagingCore
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ImagingCore (
  -- * Extension Support
  glGetARBImaging,
  gl_ARB_imaging,
  -- * Enums
  pattern GL_BLEND_COLOR,
  pattern GL_BLEND_EQUATION,
  pattern GL_CONSTANT_ALPHA,
  pattern GL_CONSTANT_COLOR,
  pattern GL_FUNC_ADD,
  pattern GL_FUNC_REVERSE_SUBTRACT,
  pattern GL_FUNC_SUBTRACT,
  pattern GL_MAX,
  pattern GL_MIN,
  pattern GL_ONE_MINUS_CONSTANT_ALPHA,
  pattern GL_ONE_MINUS_CONSTANT_COLOR,
  -- * Functions
  glBlendColor,
  glBlendEquation
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
