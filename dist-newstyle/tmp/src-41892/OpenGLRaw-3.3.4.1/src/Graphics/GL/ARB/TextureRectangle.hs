{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureRectangle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureRectangle (
  -- * Extension Support
  glGetARBTextureRectangle,
  gl_ARB_texture_rectangle,
  -- * Enums
  pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB,
  pattern GL_PROXY_TEXTURE_RECTANGLE_ARB,
  pattern GL_TEXTURE_BINDING_RECTANGLE_ARB,
  pattern GL_TEXTURE_RECTANGLE_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
