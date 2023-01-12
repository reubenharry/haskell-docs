{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureBufferObject (
  -- * Extension Support
  glGetEXTTextureBufferObject,
  gl_EXT_texture_buffer_object,
  -- * Enums
  pattern GL_MAX_TEXTURE_BUFFER_SIZE_EXT,
  pattern GL_TEXTURE_BINDING_BUFFER_EXT,
  pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT,
  pattern GL_TEXTURE_BUFFER_EXT,
  pattern GL_TEXTURE_BUFFER_FORMAT_EXT,
  -- * Functions
  glTexBufferEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
