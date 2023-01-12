{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureFilterMinmax
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureFilterMinmax (
  -- * Extension Support
  glGetARBTextureFilterMinmax,
  gl_ARB_texture_filter_minmax,
  -- * Enums
  pattern GL_TEXTURE_REDUCTION_MODE_ARB,
  pattern GL_WEIGHTED_AVERAGE_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
