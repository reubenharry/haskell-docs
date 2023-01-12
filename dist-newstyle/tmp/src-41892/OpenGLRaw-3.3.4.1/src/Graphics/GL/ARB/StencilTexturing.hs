{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.StencilTexturing
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.StencilTexturing (
  -- * Extension Support
  glGetARBStencilTexturing,
  gl_ARB_stencil_texturing,
  -- * Enums
  pattern GL_DEPTH_STENCIL_TEXTURE_MODE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
