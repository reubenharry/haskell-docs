{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TextureScaleBias
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TextureScaleBias (
  -- * Extension Support
  glGetSGIXTextureScaleBias,
  gl_SGIX_texture_scale_bias,
  -- * Enums
  pattern GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX,
  pattern GL_POST_TEXTURE_FILTER_BIAS_SGIX,
  pattern GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX,
  pattern GL_POST_TEXTURE_FILTER_SCALE_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
