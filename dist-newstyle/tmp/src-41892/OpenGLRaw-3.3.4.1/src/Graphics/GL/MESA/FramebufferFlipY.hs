{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESA.FramebufferFlipY
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESA.FramebufferFlipY (
  -- * Extension Support
  glGetMESAFramebufferFlipY,
  gl_MESA_framebuffer_flip_y,
  -- * Enums
  pattern GL_FRAMEBUFFER_FLIP_Y_MESA,
  -- * Functions
  glFramebufferParameteriMESA,
  glGetFramebufferParameterivMESA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
