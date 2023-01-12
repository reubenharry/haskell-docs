{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureFilterMinmax
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureFilterMinmax (
  -- * Extension Support
  glGetEXTTextureFilterMinmax,
  gl_EXT_texture_filter_minmax,
  -- * Enums
  pattern GL_TEXTURE_REDUCTION_MODE_EXT,
  pattern GL_WEIGHTED_AVERAGE_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
