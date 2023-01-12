{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureFilterAnisotropic
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureFilterAnisotropic (
  -- * Extension Support
  glGetARBTextureFilterAnisotropic,
  gl_ARB_texture_filter_anisotropic,
  -- * Enums
  pattern GL_MAX_TEXTURE_MAX_ANISOTROPY,
  pattern GL_TEXTURE_MAX_ANISOTROPY
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
