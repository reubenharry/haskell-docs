{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DepthTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DepthTexture (
  -- * Extension Support
  glGetARBDepthTexture,
  gl_ARB_depth_texture,
  -- * Enums
  pattern GL_DEPTH_COMPONENT16_ARB,
  pattern GL_DEPTH_COMPONENT24_ARB,
  pattern GL_DEPTH_COMPONENT32_ARB,
  pattern GL_DEPTH_TEXTURE_MODE_ARB,
  pattern GL_TEXTURE_DEPTH_SIZE_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
