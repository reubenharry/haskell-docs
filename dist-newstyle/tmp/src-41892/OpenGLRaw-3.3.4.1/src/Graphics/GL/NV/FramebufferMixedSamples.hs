{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FramebufferMixedSamples
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FramebufferMixedSamples (
  -- * Extension Support
  glGetNVFramebufferMixedSamples,
  gl_NV_framebuffer_mixed_samples,
  -- * Enums
  pattern GL_COLOR_SAMPLES_NV,
  pattern GL_COVERAGE_MODULATION_NV,
  pattern GL_COVERAGE_MODULATION_TABLE_NV,
  pattern GL_COVERAGE_MODULATION_TABLE_SIZE_NV,
  pattern GL_DEPTH_SAMPLES_NV,
  pattern GL_EFFECTIVE_RASTER_SAMPLES_EXT,
  pattern GL_MAX_RASTER_SAMPLES_EXT,
  pattern GL_MIXED_DEPTH_SAMPLES_SUPPORTED_NV,
  pattern GL_MIXED_STENCIL_SAMPLES_SUPPORTED_NV,
  pattern GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT,
  pattern GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT,
  pattern GL_RASTER_MULTISAMPLE_EXT,
  pattern GL_RASTER_SAMPLES_EXT,
  pattern GL_STENCIL_SAMPLES_NV,
  -- * Functions
  glCoverageModulationNV,
  glCoverageModulationTableNV,
  glGetCoverageModulationTableNV,
  glRasterSamplesEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
