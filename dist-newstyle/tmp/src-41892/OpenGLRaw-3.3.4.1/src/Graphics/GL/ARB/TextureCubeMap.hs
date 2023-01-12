{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureCubeMap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureCubeMap (
  -- * Extension Support
  glGetARBTextureCubeMap,
  gl_ARB_texture_cube_map,
  -- * Enums
  pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB,
  pattern GL_NORMAL_MAP_ARB,
  pattern GL_PROXY_TEXTURE_CUBE_MAP_ARB,
  pattern GL_REFLECTION_MAP_ARB,
  pattern GL_TEXTURE_BINDING_CUBE_MAP_ARB,
  pattern GL_TEXTURE_CUBE_MAP_ARB,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
