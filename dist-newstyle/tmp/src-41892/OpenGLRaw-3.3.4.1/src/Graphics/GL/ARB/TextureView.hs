{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureView
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureView (
  -- * Extension Support
  glGetARBTextureView,
  gl_ARB_texture_view,
  -- * Enums
  pattern GL_TEXTURE_IMMUTABLE_LEVELS,
  pattern GL_TEXTURE_VIEW_MIN_LAYER,
  pattern GL_TEXTURE_VIEW_MIN_LEVEL,
  pattern GL_TEXTURE_VIEW_NUM_LAYERS,
  pattern GL_TEXTURE_VIEW_NUM_LEVELS,
  -- * Functions
  glTextureView
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
