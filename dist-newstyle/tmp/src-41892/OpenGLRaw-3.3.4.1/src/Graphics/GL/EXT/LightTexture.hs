{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.LightTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.LightTexture (
  -- * Extension Support
  glGetEXTLightTexture,
  gl_EXT_light_texture,
  -- * Enums
  pattern GL_ATTENUATION_EXT,
  pattern GL_FRAGMENT_COLOR_EXT,
  pattern GL_FRAGMENT_DEPTH_EXT,
  pattern GL_FRAGMENT_MATERIAL_EXT,
  pattern GL_FRAGMENT_NORMAL_EXT,
  pattern GL_SHADOW_ATTENUATION_EXT,
  pattern GL_TEXTURE_APPLICATION_MODE_EXT,
  pattern GL_TEXTURE_LIGHT_EXT,
  pattern GL_TEXTURE_MATERIAL_FACE_EXT,
  pattern GL_TEXTURE_MATERIAL_PARAMETER_EXT,
  -- * Functions
  glApplyTextureEXT,
  glTextureLightEXT,
  glTextureMaterialEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
