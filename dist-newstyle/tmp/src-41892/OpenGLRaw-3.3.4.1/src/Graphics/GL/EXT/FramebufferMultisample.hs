{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.FramebufferMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.FramebufferMultisample (
  -- * Extension Support
  glGetEXTFramebufferMultisample,
  gl_EXT_framebuffer_multisample,
  -- * Enums
  pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT,
  pattern GL_MAX_SAMPLES_EXT,
  pattern GL_RENDERBUFFER_SAMPLES_EXT,
  -- * Functions
  glRenderbufferStorageMultisampleEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
