{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FogDistance
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FogDistance (
  -- * Extension Support
  glGetNVFogDistance,
  gl_NV_fog_distance,
  -- * Enums
  pattern GL_EYE_PLANE,
  pattern GL_EYE_PLANE_ABSOLUTE_NV,
  pattern GL_EYE_RADIAL_NV,
  pattern GL_FOG_DISTANCE_MODE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
