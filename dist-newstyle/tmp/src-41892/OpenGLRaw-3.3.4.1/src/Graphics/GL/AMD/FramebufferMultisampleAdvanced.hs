{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.FramebufferMultisampleAdvanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.FramebufferMultisampleAdvanced (
  -- * Extension Support
  glGetAMDFramebufferMultisampleAdvanced,
  gl_AMD_framebuffer_multisample_advanced,
  -- * Enums
  pattern GL_MAX_COLOR_FRAMEBUFFER_SAMPLES_AMD,
  pattern GL_MAX_COLOR_FRAMEBUFFER_STORAGE_SAMPLES_AMD,
  pattern GL_MAX_DEPTH_STENCIL_FRAMEBUFFER_SAMPLES_AMD,
  pattern GL_NUM_SUPPORTED_MULTISAMPLE_MODES_AMD,
  pattern GL_RENDERBUFFER_STORAGE_SAMPLES_AMD,
  pattern GL_SUPPORTED_MULTISAMPLE_MODES_AMD,
  -- * Functions
  glNamedRenderbufferStorageMultisampleAdvancedAMD,
  glRenderbufferStorageMultisampleAdvancedAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
