{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.FragmentLighting
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.FragmentLighting (
  -- * Extension Support
  glGetSGIXFragmentLighting,
  gl_SGIX_fragment_lighting,
  -- * Enums
  pattern GL_CURRENT_RASTER_NORMAL_SGIX,
  pattern GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX,
  pattern GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX,
  pattern GL_FRAGMENT_COLOR_MATERIAL_SGIX,
  pattern GL_FRAGMENT_LIGHT0_SGIX,
  pattern GL_FRAGMENT_LIGHT1_SGIX,
  pattern GL_FRAGMENT_LIGHT2_SGIX,
  pattern GL_FRAGMENT_LIGHT3_SGIX,
  pattern GL_FRAGMENT_LIGHT4_SGIX,
  pattern GL_FRAGMENT_LIGHT5_SGIX,
  pattern GL_FRAGMENT_LIGHT6_SGIX,
  pattern GL_FRAGMENT_LIGHT7_SGIX,
  pattern GL_FRAGMENT_LIGHTING_SGIX,
  pattern GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX,
  pattern GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX,
  pattern GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX,
  pattern GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX,
  pattern GL_LIGHT_ENV_MODE_SGIX,
  pattern GL_MAX_ACTIVE_LIGHTS_SGIX,
  pattern GL_MAX_FRAGMENT_LIGHTS_SGIX,
  -- * Functions
  glFragmentColorMaterialSGIX,
  glFragmentLightModelfSGIX,
  glFragmentLightModelfvSGIX,
  glFragmentLightModeliSGIX,
  glFragmentLightModelivSGIX,
  glFragmentLightfSGIX,
  glFragmentLightfvSGIX,
  glFragmentLightiSGIX,
  glFragmentLightivSGIX,
  glFragmentMaterialfSGIX,
  glFragmentMaterialfvSGIX,
  glFragmentMaterialiSGIX,
  glFragmentMaterialivSGIX,
  glGetFragmentLightfvSGIX,
  glGetFragmentLightivSGIX,
  glGetFragmentMaterialfvSGIX,
  glGetFragmentMaterialivSGIX,
  glLightEnviSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
