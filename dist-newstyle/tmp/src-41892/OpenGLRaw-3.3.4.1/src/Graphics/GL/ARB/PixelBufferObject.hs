{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.PixelBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.PixelBufferObject (
  -- * Extension Support
  glGetARBPixelBufferObject,
  gl_ARB_pixel_buffer_object,
  -- * Enums
  pattern GL_PIXEL_PACK_BUFFER_ARB,
  pattern GL_PIXEL_PACK_BUFFER_BINDING_ARB,
  pattern GL_PIXEL_UNPACK_BUFFER_ARB,
  pattern GL_PIXEL_UNPACK_BUFFER_BINDING_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
