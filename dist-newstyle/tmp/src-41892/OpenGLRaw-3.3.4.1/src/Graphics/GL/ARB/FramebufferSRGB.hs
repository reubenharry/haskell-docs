{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.FramebufferSRGB
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.FramebufferSRGB (
  -- * Extension Support
  glGetARBFramebufferSRGB,
  gl_ARB_framebuffer_sRGB,
  -- * Enums
  pattern GL_FRAMEBUFFER_SRGB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
