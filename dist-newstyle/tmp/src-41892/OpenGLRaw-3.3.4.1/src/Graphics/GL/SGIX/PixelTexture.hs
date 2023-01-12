{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.PixelTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.PixelTexture (
  -- * Extension Support
  glGetSGIXPixelTexture,
  gl_SGIX_pixel_texture,
  -- * Enums
  pattern GL_PIXEL_TEX_GEN_MODE_SGIX,
  pattern GL_PIXEL_TEX_GEN_SGIX,
  -- * Functions
  glPixelTexGenSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
