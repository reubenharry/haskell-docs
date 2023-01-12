{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ShaderFramebufferFetchNonCoherent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ShaderFramebufferFetchNonCoherent (
  -- * Extension Support
  glGetEXTShaderFramebufferFetchNonCoherent,
  gl_EXT_shader_framebuffer_fetch_non_coherent,
  -- * Enums
  pattern GL_FRAGMENT_SHADER_DISCARDS_SAMPLES_EXT,
  -- * Functions
  glFramebufferFetchBarrierEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
