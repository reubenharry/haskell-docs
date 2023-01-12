{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.MultisampleCoverage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.MultisampleCoverage (
  -- * Extension Support
  glGetNVMultisampleCoverage,
  gl_NV_multisample_coverage,
  -- * Enums
  pattern GL_COLOR_SAMPLES_NV,
  pattern GL_SAMPLES_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
