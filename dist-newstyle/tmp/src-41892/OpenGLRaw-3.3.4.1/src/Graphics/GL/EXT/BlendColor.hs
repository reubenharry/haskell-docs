{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BlendColor
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BlendColor (
  -- * Extension Support
  glGetEXTBlendColor,
  gl_EXT_blend_color,
  -- * Enums
  pattern GL_BLEND_COLOR_EXT,
  pattern GL_CONSTANT_ALPHA_EXT,
  pattern GL_CONSTANT_COLOR_EXT,
  pattern GL_ONE_MINUS_CONSTANT_ALPHA_EXT,
  pattern GL_ONE_MINUS_CONSTANT_COLOR_EXT,
  -- * Functions
  glBlendColorEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
