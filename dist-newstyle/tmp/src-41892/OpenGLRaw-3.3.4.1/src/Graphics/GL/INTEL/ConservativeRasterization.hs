{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.ConservativeRasterization
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.ConservativeRasterization (
  -- * Extension Support
  glGetINTELConservativeRasterization,
  gl_INTEL_conservative_rasterization,
  -- * Enums
  pattern GL_CONSERVATIVE_RASTERIZATION_INTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
