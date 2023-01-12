{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureRectangle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureRectangle (
  -- * Extension Support
  glGetNVTextureRectangle,
  gl_NV_texture_rectangle,
  -- * Enums
  pattern GL_MAX_RECTANGLE_TEXTURE_SIZE_NV,
  pattern GL_PROXY_TEXTURE_RECTANGLE_NV,
  pattern GL_TEXTURE_BINDING_RECTANGLE_NV,
  pattern GL_TEXTURE_RECTANGLE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
