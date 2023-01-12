{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.SamplePositions
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.SamplePositions (
  -- * Extension Support
  glGetAMDSamplePositions,
  gl_AMD_sample_positions,
  -- * Enums
  pattern GL_SUBSAMPLE_DISTANCE_AMD,
  -- * Functions
  glSetMultisamplefvAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
