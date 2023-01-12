{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureShader3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureShader3 (
  -- * Extension Support
  glGetNVTextureShader3,
  gl_NV_texture_shader3,
  -- * Enums
  pattern GL_DEPENDENT_HILO_TEXTURE_2D_NV,
  pattern GL_DEPENDENT_RGB_TEXTURE_3D_NV,
  pattern GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV,
  pattern GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV,
  pattern GL_DOT_PRODUCT_PASS_THROUGH_NV,
  pattern GL_DOT_PRODUCT_TEXTURE_1D_NV,
  pattern GL_FORCE_BLUE_TO_ONE_NV,
  pattern GL_HILO8_NV,
  pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV,
  pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV,
  pattern GL_OFFSET_HILO_TEXTURE_2D_NV,
  pattern GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV,
  pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV,
  pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV,
  pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV,
  pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV,
  pattern GL_SIGNED_HILO8_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
