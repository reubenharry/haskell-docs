{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.RasterMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.RasterMultisample (
  -- * Extension Support
  glGetEXTRasterMultisample,
  gl_EXT_raster_multisample,
  -- * Enums
  pattern GL_EFFECTIVE_RASTER_SAMPLES_EXT,
  pattern GL_MAX_RASTER_SAMPLES_EXT,
  pattern GL_MULTISAMPLE_RASTERIZATION_ALLOWED_EXT,
  pattern GL_RASTER_FIXED_SAMPLE_LOCATIONS_EXT,
  pattern GL_RASTER_MULTISAMPLE_EXT,
  pattern GL_RASTER_SAMPLES_EXT,
  -- * Functions
  glRasterSamplesEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
