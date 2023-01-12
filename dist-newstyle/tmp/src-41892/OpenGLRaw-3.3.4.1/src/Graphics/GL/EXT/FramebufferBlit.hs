{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FramebufferBlit
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FramebufferBlit (
  -- * Extension Support
  glGetEXTFramebufferBlit,
  gl_EXT_framebuffer_blit,
  -- * Enums
  pattern GL_DRAW_FRAMEBUFFER_BINDING_EXT,
  pattern GL_DRAW_FRAMEBUFFER_EXT,
  pattern GL_READ_FRAMEBUFFER_BINDING_EXT,
  pattern GL_READ_FRAMEBUFFER_EXT,
  -- * Functions
  glBlitFramebufferEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
