{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.BlendEquationAdvanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.BlendEquationAdvanced (
  -- * Extension Support
  glGetKHRBlendEquationAdvanced,
  gl_KHR_blend_equation_advanced,
  -- * Enums
  pattern GL_COLORBURN_KHR,
  pattern GL_COLORDODGE_KHR,
  pattern GL_DARKEN_KHR,
  pattern GL_DIFFERENCE_KHR,
  pattern GL_EXCLUSION_KHR,
  pattern GL_HARDLIGHT_KHR,
  pattern GL_HSL_COLOR_KHR,
  pattern GL_HSL_HUE_KHR,
  pattern GL_HSL_LUMINOSITY_KHR,
  pattern GL_HSL_SATURATION_KHR,
  pattern GL_LIGHTEN_KHR,
  pattern GL_MULTIPLY_KHR,
  pattern GL_OVERLAY_KHR,
  pattern GL_SCREEN_KHR,
  pattern GL_SOFTLIGHT_KHR,
  -- * Functions
  glBlendBarrierKHR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
