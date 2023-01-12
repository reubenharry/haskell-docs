{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FramebufferSRGB
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FramebufferSRGB (
  -- * Extension Support
  glGetEXTFramebufferSRGB,
  gl_EXT_framebuffer_sRGB,
  -- * Enums
  pattern GL_FRAMEBUFFER_SRGB_CAPABLE_EXT,
  pattern GL_FRAMEBUFFER_SRGB_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
