{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureCubeMap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureCubeMap (
  -- * Extension Support
  glGetEXTTextureCubeMap,
  gl_EXT_texture_cube_map,
  -- * Enums
  pattern GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT,
  pattern GL_NORMAL_MAP_EXT,
  pattern GL_PROXY_TEXTURE_CUBE_MAP_EXT,
  pattern GL_REFLECTION_MAP_EXT,
  pattern GL_TEXTURE_BINDING_CUBE_MAP_EXT,
  pattern GL_TEXTURE_CUBE_MAP_EXT,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT,
  pattern GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT,
  pattern GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
