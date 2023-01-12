{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureRGB10A2UI
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureRGB10A2UI (
  -- * Extension Support
  glGetARBTextureRGB10A2UI,
  gl_ARB_texture_rgb10_a2ui,
  -- * Enums
  pattern GL_RGB10_A2UI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
