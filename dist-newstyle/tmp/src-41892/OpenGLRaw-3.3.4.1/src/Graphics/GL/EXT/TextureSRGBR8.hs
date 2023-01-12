{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureSRGBR8
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureSRGBR8 (
  -- * Extension Support
  glGetEXTTextureSRGBR8,
  gl_EXT_texture_sRGB_R8,
  -- * Enums
  pattern GL_SR8_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
