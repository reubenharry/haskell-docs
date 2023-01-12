{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.PointParameters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.PointParameters (
  -- * Extension Support
  glGetSGISPointParameters,
  gl_SGIS_point_parameters,
  -- * Enums
  pattern GL_DISTANCE_ATTENUATION_SGIS,
  pattern GL_POINT_FADE_THRESHOLD_SIZE_SGIS,
  pattern GL_POINT_SIZE_MAX_SGIS,
  pattern GL_POINT_SIZE_MIN_SGIS,
  -- * Functions
  glPointParameterfSGIS,
  glPointParameterfvSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
