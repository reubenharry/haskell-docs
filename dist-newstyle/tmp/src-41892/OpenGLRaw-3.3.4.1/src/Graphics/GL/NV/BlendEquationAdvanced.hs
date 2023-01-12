{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.BlendEquationAdvanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.BlendEquationAdvanced (
  -- * Extension Support
  glGetNVBlendEquationAdvanced,
  gl_NV_blend_equation_advanced,
  -- * Enums
  pattern GL_BLEND_OVERLAP_NV,
  pattern GL_BLEND_PREMULTIPLIED_SRC_NV,
  pattern GL_BLUE_NV,
  pattern GL_COLORBURN_NV,
  pattern GL_COLORDODGE_NV,
  pattern GL_CONJOINT_NV,
  pattern GL_CONTRAST_NV,
  pattern GL_DARKEN_NV,
  pattern GL_DIFFERENCE_NV,
  pattern GL_DISJOINT_NV,
  pattern GL_DST_ATOP_NV,
  pattern GL_DST_IN_NV,
  pattern GL_DST_NV,
  pattern GL_DST_OUT_NV,
  pattern GL_DST_OVER_NV,
  pattern GL_EXCLUSION_NV,
  pattern GL_GREEN_NV,
  pattern GL_HARDLIGHT_NV,
  pattern GL_HARDMIX_NV,
  pattern GL_HSL_COLOR_NV,
  pattern GL_HSL_HUE_NV,
  pattern GL_HSL_LUMINOSITY_NV,
  pattern GL_HSL_SATURATION_NV,
  pattern GL_INVERT,
  pattern GL_INVERT_OVG_NV,
  pattern GL_INVERT_RGB_NV,
  pattern GL_LIGHTEN_NV,
  pattern GL_LINEARBURN_NV,
  pattern GL_LINEARDODGE_NV,
  pattern GL_LINEARLIGHT_NV,
  pattern GL_MINUS_CLAMPED_NV,
  pattern GL_MINUS_NV,
  pattern GL_MULTIPLY_NV,
  pattern GL_OVERLAY_NV,
  pattern GL_PINLIGHT_NV,
  pattern GL_PLUS_CLAMPED_ALPHA_NV,
  pattern GL_PLUS_CLAMPED_NV,
  pattern GL_PLUS_DARKER_NV,
  pattern GL_PLUS_NV,
  pattern GL_RED_NV,
  pattern GL_SCREEN_NV,
  pattern GL_SOFTLIGHT_NV,
  pattern GL_SRC_ATOP_NV,
  pattern GL_SRC_IN_NV,
  pattern GL_SRC_NV,
  pattern GL_SRC_OUT_NV,
  pattern GL_SRC_OVER_NV,
  pattern GL_UNCORRELATED_NV,
  pattern GL_VIVIDLIGHT_NV,
  pattern GL_XOR_NV,
  pattern GL_ZERO,
  -- * Functions
  glBlendBarrierNV,
  glBlendParameteriNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
