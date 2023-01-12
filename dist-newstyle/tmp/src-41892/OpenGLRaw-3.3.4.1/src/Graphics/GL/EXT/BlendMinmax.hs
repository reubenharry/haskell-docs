{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BlendMinmax
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BlendMinmax (
  -- * Extension Support
  glGetEXTBlendMinmax,
  gl_EXT_blend_minmax,
  -- * Enums
  pattern GL_BLEND_EQUATION_EXT,
  pattern GL_FUNC_ADD_EXT,
  pattern GL_MAX_EXT,
  pattern GL_MIN_EXT,
  -- * Functions
  glBlendEquationEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
