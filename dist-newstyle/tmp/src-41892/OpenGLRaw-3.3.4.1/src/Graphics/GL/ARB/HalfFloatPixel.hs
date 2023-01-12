{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.HalfFloatPixel
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.HalfFloatPixel (
  -- * Extension Support
  glGetARBHalfFloatPixel,
  gl_ARB_half_float_pixel,
  -- * Types
  GLhalfARB,
  -- * Enums
  pattern GL_HALF_FLOAT_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Types
import Graphics.GL.Tokens
