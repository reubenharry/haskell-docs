{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureSwizzle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureSwizzle (
  -- * Extension Support
  glGetEXTTextureSwizzle,
  gl_EXT_texture_swizzle,
  -- * Enums
  pattern GL_TEXTURE_SWIZZLE_A_EXT,
  pattern GL_TEXTURE_SWIZZLE_B_EXT,
  pattern GL_TEXTURE_SWIZZLE_G_EXT,
  pattern GL_TEXTURE_SWIZZLE_RGBA_EXT,
  pattern GL_TEXTURE_SWIZZLE_R_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
