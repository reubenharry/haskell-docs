{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PalettedTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PalettedTexture (
  -- * Extension Support
  glGetEXTPalettedTexture,
  gl_EXT_paletted_texture,
  -- * Enums
  pattern GL_COLOR_INDEX12_EXT,
  pattern GL_COLOR_INDEX16_EXT,
  pattern GL_COLOR_INDEX1_EXT,
  pattern GL_COLOR_INDEX2_EXT,
  pattern GL_COLOR_INDEX4_EXT,
  pattern GL_COLOR_INDEX8_EXT,
  pattern GL_TEXTURE_INDEX_SIZE_EXT,
  -- * Functions
  glColorTableEXT,
  glGetColorTableEXT,
  glGetColorTableParameterfvEXT,
  glGetColorTableParameterivEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
