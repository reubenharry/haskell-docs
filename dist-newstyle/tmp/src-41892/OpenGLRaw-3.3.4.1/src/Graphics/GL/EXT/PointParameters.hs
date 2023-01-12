{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PointParameters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PointParameters (
  -- * Extension Support
  glGetEXTPointParameters,
  gl_EXT_point_parameters,
  -- * Enums
  pattern GL_DISTANCE_ATTENUATION_EXT,
  pattern GL_POINT_FADE_THRESHOLD_SIZE_EXT,
  pattern GL_POINT_SIZE_MAX_EXT,
  pattern GL_POINT_SIZE_MIN_EXT,
  -- * Functions
  glPointParameterfEXT,
  glPointParameterfvEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
