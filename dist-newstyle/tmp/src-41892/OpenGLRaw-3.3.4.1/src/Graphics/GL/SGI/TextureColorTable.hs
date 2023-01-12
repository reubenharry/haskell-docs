{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGI.TextureColorTable
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGI.TextureColorTable (
  -- * Extension Support
  glGetSGITextureColorTable,
  gl_SGI_texture_color_table,
  -- * Enums
  pattern GL_PROXY_TEXTURE_COLOR_TABLE_SGI,
  pattern GL_TEXTURE_COLOR_TABLE_SGI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
