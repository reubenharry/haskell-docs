{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.Multisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.Multisample (
  -- * Extension Support
  glGetARBMultisample,
  gl_ARB_multisample,
  -- * Enums
  pattern GL_MULTISAMPLE_ARB,
  pattern GL_MULTISAMPLE_BIT_ARB,
  pattern GL_SAMPLES_ARB,
  pattern GL_SAMPLE_ALPHA_TO_COVERAGE_ARB,
  pattern GL_SAMPLE_ALPHA_TO_ONE_ARB,
  pattern GL_SAMPLE_BUFFERS_ARB,
  pattern GL_SAMPLE_COVERAGE_ARB,
  pattern GL_SAMPLE_COVERAGE_INVERT_ARB,
  pattern GL_SAMPLE_COVERAGE_VALUE_ARB,
  -- * Functions
  glSampleCoverageARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
