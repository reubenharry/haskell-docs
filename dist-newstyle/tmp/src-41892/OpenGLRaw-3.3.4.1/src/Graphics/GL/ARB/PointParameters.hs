{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.PointParameters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.PointParameters (
  -- * Extension Support
  glGetARBPointParameters,
  gl_ARB_point_parameters,
  -- * Enums
  pattern GL_POINT_DISTANCE_ATTENUATION_ARB,
  pattern GL_POINT_FADE_THRESHOLD_SIZE_ARB,
  pattern GL_POINT_SIZE_MAX_ARB,
  pattern GL_POINT_SIZE_MIN_ARB,
  -- * Functions
  glPointParameterfARB,
  glPointParameterfvARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
