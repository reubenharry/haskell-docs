{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ExplicitUniformLocation
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ExplicitUniformLocation (
  -- * Extension Support
  glGetARBExplicitUniformLocation,
  gl_ARB_explicit_uniform_location,
  -- * Enums
  pattern GL_MAX_UNIFORM_LOCATIONS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
