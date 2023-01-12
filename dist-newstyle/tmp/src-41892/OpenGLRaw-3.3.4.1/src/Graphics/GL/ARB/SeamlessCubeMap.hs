{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SeamlessCubeMap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SeamlessCubeMap (
  -- * Extension Support
  glGetARBSeamlessCubeMap,
  gl_ARB_seamless_cube_map,
  -- * Enums
  pattern GL_TEXTURE_CUBE_MAP_SEAMLESS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
