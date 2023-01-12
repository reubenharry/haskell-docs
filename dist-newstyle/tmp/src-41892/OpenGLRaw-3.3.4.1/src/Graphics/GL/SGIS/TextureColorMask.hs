{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureColorMask
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureColorMask (
  -- * Extension Support
  glGetSGISTextureColorMask,
  gl_SGIS_texture_color_mask,
  -- * Enums
  pattern GL_TEXTURE_COLOR_WRITEMASK_SGIS,
  -- * Functions
  glTextureColorMaskSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
