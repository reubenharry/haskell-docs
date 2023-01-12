{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.AlphaToCoverageDitherControl
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.AlphaToCoverageDitherControl (
  -- * Extension Support
  glGetNVAlphaToCoverageDitherControl,
  gl_NV_alpha_to_coverage_dither_control,
  -- * Enums
  pattern GL_ALPHA_TO_COVERAGE_DITHER_DEFAULT_NV,
  pattern GL_ALPHA_TO_COVERAGE_DITHER_DISABLE_NV,
  pattern GL_ALPHA_TO_COVERAGE_DITHER_ENABLE_NV,
  pattern GL_ALPHA_TO_COVERAGE_DITHER_MODE_NV,
  -- * Functions
  glAlphaToCoverageDitherControlNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
