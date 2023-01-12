{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ShaderFramebufferFetch
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ShaderFramebufferFetch (
  -- * Extension Support
  glGetEXTShaderFramebufferFetch,
  gl_EXT_shader_framebuffer_fetch,
  -- * Enums
  pattern GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
