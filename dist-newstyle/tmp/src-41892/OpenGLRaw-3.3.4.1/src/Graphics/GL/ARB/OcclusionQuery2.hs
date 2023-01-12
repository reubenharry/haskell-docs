{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.OcclusionQuery2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.OcclusionQuery2 (
  -- * Extension Support
  glGetARBOcclusionQuery2,
  gl_ARB_occlusion_query2,
  -- * Enums
  pattern GL_ANY_SAMPLES_PASSED
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
