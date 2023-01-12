{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ES31Compatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ES31Compatibility (
  -- * Extension Support
  glGetARBES31Compatibility,
  gl_ARB_ES3_1_compatibility,
  -- * Enums
  pattern GL_BACK,
  -- * Functions
  glMemoryBarrierByRegion
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
