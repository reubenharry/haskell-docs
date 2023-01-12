{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureEnvCombine4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureEnvCombine4 (
  -- * Extension Support
  glGetNVTextureEnvCombine4,
  gl_NV_texture_env_combine4,
  -- * Enums
  pattern GL_COMBINE4_NV,
  pattern GL_OPERAND3_ALPHA_NV,
  pattern GL_OPERAND3_RGB_NV,
  pattern GL_SOURCE3_ALPHA_NV,
  pattern GL_SOURCE3_RGB_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
