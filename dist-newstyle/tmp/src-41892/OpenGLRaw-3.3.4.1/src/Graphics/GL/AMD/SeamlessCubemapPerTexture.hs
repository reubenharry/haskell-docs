{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.SeamlessCubemapPerTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.SeamlessCubemapPerTexture (
  -- * Extension Support
  glGetAMDSeamlessCubemapPerTexture,
  gl_AMD_seamless_cubemap_per_texture,
  -- * Enums
  pattern GL_TEXTURE_CUBE_MAP_SEAMLESS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
