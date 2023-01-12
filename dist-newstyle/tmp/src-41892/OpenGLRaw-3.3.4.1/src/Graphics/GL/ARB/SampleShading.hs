{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SampleShading
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SampleShading (
  -- * Extension Support
  glGetARBSampleShading,
  gl_ARB_sample_shading,
  -- * Enums
  pattern GL_MIN_SAMPLE_SHADING_VALUE_ARB,
  pattern GL_SAMPLE_SHADING_ARB,
  -- * Functions
  glMinSampleShadingARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
