{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureShader (
  -- * Extension Support
  glGetNVTextureShader,
  gl_NV_texture_shader,
  -- * Enums
  pattern GL_CONST_EYE_NV,
  pattern GL_CULL_FRAGMENT_NV,
  pattern GL_CULL_MODES_NV,
  pattern GL_DEPENDENT_AR_TEXTURE_2D_NV,
  pattern GL_DEPENDENT_GB_TEXTURE_2D_NV,
  pattern GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV,
  pattern GL_DOT_PRODUCT_DEPTH_REPLACE_NV,
  pattern GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV,
  pattern GL_DOT_PRODUCT_NV,
  pattern GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV,
  pattern GL_DOT_PRODUCT_TEXTURE_2D_NV,
  pattern GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV,
  pattern GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV,
  pattern GL_DSDT8_MAG8_INTENSITY8_NV,
  pattern GL_DSDT8_MAG8_NV,
  pattern GL_DSDT8_NV,
  pattern GL_DSDT_MAG_INTENSITY_NV,
  pattern GL_DSDT_MAG_NV,
  pattern GL_DSDT_MAG_VIB_NV,
  pattern GL_DSDT_NV,
  pattern GL_DS_BIAS_NV,
  pattern GL_DS_SCALE_NV,
  pattern GL_DT_BIAS_NV,
  pattern GL_DT_SCALE_NV,
  pattern GL_HILO16_NV,
  pattern GL_HILO_NV,
  pattern GL_HI_BIAS_NV,
  pattern GL_HI_SCALE_NV,
  pattern GL_LO_BIAS_NV,
  pattern GL_LO_SCALE_NV,
  pattern GL_MAGNITUDE_BIAS_NV,
  pattern GL_MAGNITUDE_SCALE_NV,
  pattern GL_OFFSET_TEXTURE_2D_BIAS_NV,
  pattern GL_OFFSET_TEXTURE_2D_MATRIX_NV,
  pattern GL_OFFSET_TEXTURE_2D_NV,
  pattern GL_OFFSET_TEXTURE_2D_SCALE_NV,
  pattern GL_OFFSET_TEXTURE_BIAS_NV,
  pattern GL_OFFSET_TEXTURE_MATRIX_NV,
  pattern GL_OFFSET_TEXTURE_RECTANGLE_NV,
  pattern GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV,
  pattern GL_OFFSET_TEXTURE_SCALE_NV,
  pattern GL_PASS_THROUGH_NV,
  pattern GL_PREVIOUS_TEXTURE_INPUT_NV,
  pattern GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV,
  pattern GL_SHADER_CONSISTENT_NV,
  pattern GL_SHADER_OPERATION_NV,
  pattern GL_SIGNED_ALPHA8_NV,
  pattern GL_SIGNED_ALPHA_NV,
  pattern GL_SIGNED_HILO16_NV,
  pattern GL_SIGNED_HILO_NV,
  pattern GL_SIGNED_INTENSITY8_NV,
  pattern GL_SIGNED_INTENSITY_NV,
  pattern GL_SIGNED_LUMINANCE8_ALPHA8_NV,
  pattern GL_SIGNED_LUMINANCE8_NV,
  pattern GL_SIGNED_LUMINANCE_ALPHA_NV,
  pattern GL_SIGNED_LUMINANCE_NV,
  pattern GL_SIGNED_RGB8_NV,
  pattern GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV,
  pattern GL_SIGNED_RGBA8_NV,
  pattern GL_SIGNED_RGBA_NV,
  pattern GL_SIGNED_RGB_NV,
  pattern GL_SIGNED_RGB_UNSIGNED_ALPHA_NV,
  pattern GL_TEXTURE_BORDER_VALUES_NV,
  pattern GL_TEXTURE_DS_SIZE_NV,
  pattern GL_TEXTURE_DT_SIZE_NV,
  pattern GL_TEXTURE_HI_SIZE_NV,
  pattern GL_TEXTURE_LO_SIZE_NV,
  pattern GL_TEXTURE_MAG_SIZE_NV,
  pattern GL_TEXTURE_SHADER_NV,
  pattern GL_UNSIGNED_INT_8_8_S8_S8_REV_NV,
  pattern GL_UNSIGNED_INT_S8_S8_8_8_NV,
  pattern GL_VIBRANCE_BIAS_NV,
  pattern GL_VIBRANCE_SCALE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
