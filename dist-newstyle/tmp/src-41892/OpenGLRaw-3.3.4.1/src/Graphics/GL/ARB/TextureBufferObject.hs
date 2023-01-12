{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureBufferObject (
  -- * Extension Support
  glGetARBTextureBufferObject,
  gl_ARB_texture_buffer_object,
  -- * Enums
  pattern GL_MAX_TEXTURE_BUFFER_SIZE_ARB,
  pattern GL_TEXTURE_BINDING_BUFFER_ARB,
  pattern GL_TEXTURE_BUFFER_ARB,
  pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB,
  pattern GL_TEXTURE_BUFFER_FORMAT_ARB,
  -- * Functions
  glTexBufferARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
