{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureEnvDot3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureEnvDot3 (
  -- * Extension Support
  glGetEXTTextureEnvDot3,
  gl_EXT_texture_env_dot3,
  -- * Enums
  pattern GL_DOT3_RGBA_EXT,
  pattern GL_DOT3_RGB_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
