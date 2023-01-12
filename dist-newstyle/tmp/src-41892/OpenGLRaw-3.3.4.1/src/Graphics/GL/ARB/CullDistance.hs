{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.CullDistance
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.CullDistance (
  -- * Extension Support
  glGetARBCullDistance,
  gl_ARB_cull_distance,
  -- * Enums
  pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES,
  pattern GL_MAX_CULL_DISTANCES
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
