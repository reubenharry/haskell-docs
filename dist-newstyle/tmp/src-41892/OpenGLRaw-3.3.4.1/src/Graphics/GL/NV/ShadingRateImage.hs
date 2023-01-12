{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ShadingRateImage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ShadingRateImage (
  -- * Extension Support
  glGetNVShadingRateImage,
  gl_NV_shading_rate_image,
  -- * Enums
  pattern GL_MAX_COARSE_FRAGMENT_SAMPLES_NV,
  pattern GL_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV,
  pattern GL_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV,
  pattern GL_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV,
  pattern GL_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV,
  pattern GL_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV,
  pattern GL_SHADING_RATE_IMAGE_BINDING_NV,
  pattern GL_SHADING_RATE_IMAGE_NV,
  pattern GL_SHADING_RATE_IMAGE_PALETTE_SIZE_NV,
  pattern GL_SHADING_RATE_IMAGE_TEXEL_HEIGHT_NV,
  pattern GL_SHADING_RATE_IMAGE_TEXEL_WIDTH_NV,
  pattern GL_SHADING_RATE_NO_INVOCATIONS_NV,
  pattern GL_SHADING_RATE_SAMPLE_ORDER_DEFAULT_NV,
  pattern GL_SHADING_RATE_SAMPLE_ORDER_PIXEL_MAJOR_NV,
  pattern GL_SHADING_RATE_SAMPLE_ORDER_SAMPLE_MAJOR_NV,
  -- * Functions
  glBindShadingRateImageNV,
  glGetShadingRateImagePaletteNV,
  glGetShadingRateSampleLocationivNV,
  glShadingRateImageBarrierNV,
  glShadingRateImagePaletteNV,
  glShadingRateSampleOrderCustomNV,
  glShadingRateSampleOrderNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
