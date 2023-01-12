{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureExpandNormal
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureExpandNormal (
  -- * Extension Support
  glGetNVTextureExpandNormal,
  gl_NV_texture_expand_normal,
  -- * Enums
  pattern GL_TEXTURE_UNSIGNED_REMAP_MODE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
