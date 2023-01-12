{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureShader2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureShader2 (
  -- * Extension Support
  glGetNVTextureShader2,
  gl_NV_texture_shader2,
  -- * Enums
  pattern GL_DOT_PRODUCT_TEXTURE_3D_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
