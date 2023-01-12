{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureRG
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureRG (
  -- * Extension Support
  glGetARBTextureRG,
  gl_ARB_texture_rg,
  -- * Enums
  pattern GL_R16,
  pattern GL_R16F,
  pattern GL_R16I,
  pattern GL_R16UI,
  pattern GL_R32F,
  pattern GL_R32I,
  pattern GL_R32UI,
  pattern GL_R8,
  pattern GL_R8I,
  pattern GL_R8UI,
  pattern GL_RG,
  pattern GL_RG16,
  pattern GL_RG16F,
  pattern GL_RG16I,
  pattern GL_RG16UI,
  pattern GL_RG32F,
  pattern GL_RG32I,
  pattern GL_RG32UI,
  pattern GL_RG8,
  pattern GL_RG8I,
  pattern GL_RG8UI,
  pattern GL_RG_INTEGER
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
