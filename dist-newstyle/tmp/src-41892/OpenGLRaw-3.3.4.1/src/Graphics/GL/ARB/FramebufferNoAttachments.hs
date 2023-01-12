{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.FramebufferNoAttachments
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.FramebufferNoAttachments (
  -- * Extension Support
  glGetARBFramebufferNoAttachments,
  gl_ARB_framebuffer_no_attachments,
  -- * Enums
  pattern GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS,
  pattern GL_FRAMEBUFFER_DEFAULT_HEIGHT,
  pattern GL_FRAMEBUFFER_DEFAULT_LAYERS,
  pattern GL_FRAMEBUFFER_DEFAULT_SAMPLES,
  pattern GL_FRAMEBUFFER_DEFAULT_WIDTH,
  pattern GL_MAX_FRAMEBUFFER_HEIGHT,
  pattern GL_MAX_FRAMEBUFFER_LAYERS,
  pattern GL_MAX_FRAMEBUFFER_SAMPLES,
  pattern GL_MAX_FRAMEBUFFER_WIDTH,
  -- * Functions
  glFramebufferParameteri,
  glGetFramebufferParameteriv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
