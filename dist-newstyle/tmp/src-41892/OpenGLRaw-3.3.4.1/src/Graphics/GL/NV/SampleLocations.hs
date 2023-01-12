{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.SampleLocations
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.SampleLocations (
  -- * Extension Support
  glGetNVSampleLocations,
  gl_NV_sample_locations,
  -- * Enums
  pattern GL_FRAMEBUFFER_PROGRAMMABLE_SAMPLE_LOCATIONS_NV,
  pattern GL_FRAMEBUFFER_SAMPLE_LOCATION_PIXEL_GRID_NV,
  pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_NV,
  pattern GL_PROGRAMMABLE_SAMPLE_LOCATION_TABLE_SIZE_NV,
  pattern GL_SAMPLE_LOCATION_NV,
  pattern GL_SAMPLE_LOCATION_PIXEL_GRID_HEIGHT_NV,
  pattern GL_SAMPLE_LOCATION_PIXEL_GRID_WIDTH_NV,
  pattern GL_SAMPLE_LOCATION_SUBPIXEL_BITS_NV,
  -- * Functions
  glFramebufferSampleLocationsfvNV,
  glNamedFramebufferSampleLocationsfvNV,
  glResolveDepthValuesNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
