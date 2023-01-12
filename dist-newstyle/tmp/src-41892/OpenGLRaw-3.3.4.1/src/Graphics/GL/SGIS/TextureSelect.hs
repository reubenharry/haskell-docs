{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureSelect
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureSelect (
  -- * Extension Support
  glGetSGISTextureSelect,
  gl_SGIS_texture_select,
  -- * Enums
  pattern GL_DUAL_ALPHA12_SGIS,
  pattern GL_DUAL_ALPHA16_SGIS,
  pattern GL_DUAL_ALPHA4_SGIS,
  pattern GL_DUAL_ALPHA8_SGIS,
  pattern GL_DUAL_INTENSITY12_SGIS,
  pattern GL_DUAL_INTENSITY16_SGIS,
  pattern GL_DUAL_INTENSITY4_SGIS,
  pattern GL_DUAL_INTENSITY8_SGIS,
  pattern GL_DUAL_LUMINANCE12_SGIS,
  pattern GL_DUAL_LUMINANCE16_SGIS,
  pattern GL_DUAL_LUMINANCE4_SGIS,
  pattern GL_DUAL_LUMINANCE8_SGIS,
  pattern GL_DUAL_LUMINANCE_ALPHA4_SGIS,
  pattern GL_DUAL_LUMINANCE_ALPHA8_SGIS,
  pattern GL_DUAL_TEXTURE_SELECT_SGIS,
  pattern GL_QUAD_ALPHA4_SGIS,
  pattern GL_QUAD_ALPHA8_SGIS,
  pattern GL_QUAD_INTENSITY4_SGIS,
  pattern GL_QUAD_INTENSITY8_SGIS,
  pattern GL_QUAD_LUMINANCE4_SGIS,
  pattern GL_QUAD_LUMINANCE8_SGIS,
  pattern GL_QUAD_TEXTURE_SELECT_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
