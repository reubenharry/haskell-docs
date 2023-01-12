{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.TextureRange
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.TextureRange (
  -- * Extension Support
  glGetAPPLETextureRange,
  gl_APPLE_texture_range,
  -- * Enums
  pattern GL_STORAGE_CACHED_APPLE,
  pattern GL_STORAGE_PRIVATE_APPLE,
  pattern GL_STORAGE_SHARED_APPLE,
  pattern GL_TEXTURE_RANGE_LENGTH_APPLE,
  pattern GL_TEXTURE_RANGE_POINTER_APPLE,
  pattern GL_TEXTURE_STORAGE_HINT_APPLE,
  -- * Functions
  glGetTexParameterPointervAPPLE,
  glTextureRangeAPPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
