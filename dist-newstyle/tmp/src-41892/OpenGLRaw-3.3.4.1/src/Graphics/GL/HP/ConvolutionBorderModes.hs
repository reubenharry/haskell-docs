{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.HP.ConvolutionBorderModes
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.HP.ConvolutionBorderModes (
  -- * Extension Support
  glGetHPConvolutionBorderModes,
  gl_HP_convolution_border_modes,
  -- * Enums
  pattern GL_CONSTANT_BORDER_HP,
  pattern GL_CONVOLUTION_BORDER_COLOR_HP,
  pattern GL_IGNORE_BORDER_HP,
  pattern GL_REPLICATE_BORDER_HP
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
