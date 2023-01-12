{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TextureAddEnv
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TextureAddEnv (
  -- * Extension Support
  glGetSGIXTextureAddEnv,
  gl_SGIX_texture_add_env,
  -- * Enums
  pattern GL_TEXTURE_ENV_BIAS_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
