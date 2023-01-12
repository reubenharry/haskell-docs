{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureStencil8
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureStencil8 (
  -- * Extension Support
  glGetARBTextureStencil8,
  gl_ARB_texture_stencil8,
  -- * Enums
  pattern GL_STENCIL_INDEX,
  pattern GL_STENCIL_INDEX8
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
