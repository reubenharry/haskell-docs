{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureStorage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureStorage (
  -- * Extension Support
  glGetARBTextureStorage,
  gl_ARB_texture_storage,
  -- * Enums
  pattern GL_TEXTURE_IMMUTABLE_FORMAT,
  -- * Functions
  glTexStorage1D,
  glTexStorage2D,
  glTexStorage3D
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
