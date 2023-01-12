{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.HalfFloatVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.HalfFloatVertex (
  -- * Extension Support
  glGetARBHalfFloatVertex,
  gl_ARB_half_float_vertex,
  -- * Types
  GLhalf,
  -- * Enums
  pattern GL_HALF_FLOAT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Types
import Graphics.GL.Tokens
