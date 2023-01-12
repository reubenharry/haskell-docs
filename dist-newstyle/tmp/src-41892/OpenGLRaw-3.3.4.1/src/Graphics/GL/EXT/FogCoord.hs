{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FogCoord
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FogCoord (
  -- * Extension Support
  glGetEXTFogCoord,
  gl_EXT_fog_coord,
  -- * Enums
  pattern GL_CURRENT_FOG_COORDINATE_EXT,
  pattern GL_FOG_COORDINATE_ARRAY_EXT,
  pattern GL_FOG_COORDINATE_ARRAY_POINTER_EXT,
  pattern GL_FOG_COORDINATE_ARRAY_STRIDE_EXT,
  pattern GL_FOG_COORDINATE_ARRAY_TYPE_EXT,
  pattern GL_FOG_COORDINATE_EXT,
  pattern GL_FOG_COORDINATE_SOURCE_EXT,
  pattern GL_FRAGMENT_DEPTH_EXT,
  -- * Functions
  glFogCoordPointerEXT,
  glFogCoorddEXT,
  glFogCoorddvEXT,
  glFogCoordfEXT,
  glFogCoordfvEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
