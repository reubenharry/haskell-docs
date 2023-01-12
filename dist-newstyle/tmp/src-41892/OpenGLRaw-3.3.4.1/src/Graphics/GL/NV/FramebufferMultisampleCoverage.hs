{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FramebufferMultisampleCoverage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FramebufferMultisampleCoverage (
  -- * Extension Support
  glGetNVFramebufferMultisampleCoverage,
  gl_NV_framebuffer_multisample_coverage,
  -- * Enums
  pattern GL_MAX_MULTISAMPLE_COVERAGE_MODES_NV,
  pattern GL_MULTISAMPLE_COVERAGE_MODES_NV,
  pattern GL_RENDERBUFFER_COLOR_SAMPLES_NV,
  pattern GL_RENDERBUFFER_COVERAGE_SAMPLES_NV,
  -- * Functions
  glRenderbufferStorageMultisampleCoverageNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
