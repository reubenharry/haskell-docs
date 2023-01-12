{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureBufferObjectRGB32
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureBufferObjectRGB32 (
  -- * Extension Support
  glGetARBTextureBufferObjectRGB32,
  gl_ARB_texture_buffer_object_rgb32,
  -- * Enums
  pattern GL_RGB32F,
  pattern GL_RGB32I,
  pattern GL_RGB32UI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
