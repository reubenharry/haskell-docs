{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.ConvolutionAccuracy
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.ConvolutionAccuracy (
  -- * Extension Support
  glGetSGIXConvolutionAccuracy,
  gl_SGIX_convolution_accuracy,
  -- * Enums
  pattern GL_CONVOLUTION_HINT_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
