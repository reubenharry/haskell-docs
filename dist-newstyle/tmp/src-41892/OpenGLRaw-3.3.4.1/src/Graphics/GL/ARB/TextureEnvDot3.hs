{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureEnvDot3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureEnvDot3 (
  -- * Extension Support
  glGetARBTextureEnvDot3,
  gl_ARB_texture_env_dot3,
  -- * Enums
  pattern GL_DOT3_RGBA_ARB,
  pattern GL_DOT3_RGB_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
