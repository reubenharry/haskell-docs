{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ShaderImageLoadStore
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ShaderImageLoadStore (
  -- * Extension Support
  glGetEXTShaderImageLoadStore,
  gl_EXT_shader_image_load_store,
  -- * Enums
  pattern GL_ALL_BARRIER_BITS_EXT,
  pattern GL_ATOMIC_COUNTER_BARRIER_BIT_EXT,
  pattern GL_BUFFER_UPDATE_BARRIER_BIT_EXT,
  pattern GL_COMMAND_BARRIER_BIT_EXT,
  pattern GL_ELEMENT_ARRAY_BARRIER_BIT_EXT,
  pattern GL_FRAMEBUFFER_BARRIER_BIT_EXT,
  pattern GL_IMAGE_1D_ARRAY_EXT,
  pattern GL_IMAGE_1D_EXT,
  pattern GL_IMAGE_2D_ARRAY_EXT,
  pattern GL_IMAGE_2D_EXT,
  pattern GL_IMAGE_2D_MULTISAMPLE_ARRAY_EXT,
  pattern GL_IMAGE_2D_MULTISAMPLE_EXT,
  pattern GL_IMAGE_2D_RECT_EXT,
  pattern GL_IMAGE_3D_EXT,
  pattern GL_IMAGE_BINDING_ACCESS_EXT,
  pattern GL_IMAGE_BINDING_FORMAT_EXT,
  pattern GL_IMAGE_BINDING_LAYERED_EXT,
  pattern GL_IMAGE_BINDING_LAYER_EXT,
  pattern GL_IMAGE_BINDING_LEVEL_EXT,
  pattern GL_IMAGE_BINDING_NAME_EXT,
  pattern GL_IMAGE_BUFFER_EXT,
  pattern GL_IMAGE_CUBE_EXT,
  pattern GL_IMAGE_CUBE_MAP_ARRAY_EXT,
  pattern GL_INT_IMAGE_1D_ARRAY_EXT,
  pattern GL_INT_IMAGE_1D_EXT,
  pattern GL_INT_IMAGE_2D_ARRAY_EXT,
  pattern GL_INT_IMAGE_2D_EXT,
  pattern GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT,
  pattern GL_INT_IMAGE_2D_MULTISAMPLE_EXT,
  pattern GL_INT_IMAGE_2D_RECT_EXT,
  pattern GL_INT_IMAGE_3D_EXT,
  pattern GL_INT_IMAGE_BUFFER_EXT,
  pattern GL_INT_IMAGE_CUBE_EXT,
  pattern GL_INT_IMAGE_CUBE_MAP_ARRAY_EXT,
  pattern GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS_EXT,
  pattern GL_MAX_IMAGE_SAMPLES_EXT,
  pattern GL_MAX_IMAGE_UNITS_EXT,
  pattern GL_PIXEL_BUFFER_BARRIER_BIT_EXT,
  pattern GL_SHADER_IMAGE_ACCESS_BARRIER_BIT_EXT,
  pattern GL_TEXTURE_FETCH_BARRIER_BIT_EXT,
  pattern GL_TEXTURE_UPDATE_BARRIER_BIT_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BARRIER_BIT_EXT,
  pattern GL_UNIFORM_BARRIER_BIT_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_1D_ARRAY_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_1D_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_2D_ARRAY_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_2D_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_2D_RECT_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_3D_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_BUFFER_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_CUBE_EXT,
  pattern GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY_EXT,
  pattern GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT_EXT,
  -- * Functions
  glBindImageTextureEXT,
  glMemoryBarrierEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
