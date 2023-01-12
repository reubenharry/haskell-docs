{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SampleLocations
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SampleLocations (
  -- * Extension Support
  glGetARBSampleLocations,
  gl_ARB_sample_locations,
  -- * Enums
  pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_ARB,
  pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_ARB,
  pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_ARB,
  pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_ARB,
  pattern GL_SAMPLE_LOCATION_ARB,
  pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_ARB,
  pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_ARB,
  pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_ARB,
  -- * Functions
  glEvaluateDepthValuesARB,
  glFramebufferSampleLocationsfvARB,
  glNamedFramebufferSampleLocationsfvARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
