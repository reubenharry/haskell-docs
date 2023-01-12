{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.ConvolutionBorderModes
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.ConvolutionBorderModes (
  -- * Extension Support
  glGetSUNConvolutionBorderModes,
  gl_SUN_convolution_border_modes,
  -- * Enums
  pattern GL_WRAP_BORDER_SUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
