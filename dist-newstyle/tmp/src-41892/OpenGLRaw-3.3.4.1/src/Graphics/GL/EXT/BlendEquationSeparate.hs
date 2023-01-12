{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BlendEquationSeparate
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BlendEquationSeparate (
  -- * Extension Support
  glGetEXTBlendEquationSeparate,
  gl_EXT_blend_equation_separate,
  -- * Enums
  pattern GL_BLEND_EQUATION_ALPHA_EXT,
  pattern GL_BLEND_EQUATION_RGB_EXT,
  -- * Functions
  glBlendEquationSeparateEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
