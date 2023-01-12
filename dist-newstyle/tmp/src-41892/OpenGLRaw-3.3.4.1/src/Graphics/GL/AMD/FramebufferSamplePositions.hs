{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.FramebufferSamplePositions
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.FramebufferSamplePositions (
  -- * Extension Support
  glGetAMDFramebufferSamplePositions,
  gl_AMD_framebuffer_sample_positions,
  -- * Enums
  pattern GL_ALL_PIXELS_AMD,
  pattern GL_PIXELS_PER_SAMPLE_PATTERN_X_AMD,
  pattern GL_PIXELS_PER_SAMPLE_PATTERN_Y_AMD,
  pattern GL_SUBSAMPLE_DISTANCE_AMD,
  -- * Functions
  glFramebufferSamplePositionsfvAMD,
  glGetFramebufferParameterfvAMD,
  glGetNamedFramebufferParameterfvAMD,
  glNamedFramebufferSamplePositionsfvAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
