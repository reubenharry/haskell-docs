{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureBufferRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureBufferRange (
  -- * Extension Support
  glGetARBTextureBufferRange,
  gl_ARB_texture_buffer_range,
  -- * Enums
  pattern GL_TEXTURE_BUFFER_OFFSET,
  pattern GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT,
  pattern GL_TEXTURE_BUFFER_SIZE,
  -- * Functions
  glTexBufferRange
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
