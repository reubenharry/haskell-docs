{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureSwizzle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureSwizzle (
  -- * Extension Support
  glGetARBTextureSwizzle,
  gl_ARB_texture_swizzle,
  -- * Enums
  pattern GL_TEXTURE_SWIZZLE_A,
  pattern GL_TEXTURE_SWIZZLE_B,
  pattern GL_TEXTURE_SWIZZLE_G,
  pattern GL_TEXTURE_SWIZZLE_R,
  pattern GL_TEXTURE_SWIZZLE_RGBA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
