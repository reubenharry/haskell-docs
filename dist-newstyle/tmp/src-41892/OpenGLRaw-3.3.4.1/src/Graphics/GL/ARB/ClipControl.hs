{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ClipControl
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ClipControl (
  -- * Extension Support
  glGetARBClipControl,
  gl_ARB_clip_control,
  -- * Enums
  pattern GL_CLIP_DEPTH_MODE,
  pattern GL_CLIP_ORIGIN,
  pattern GL_LOWER_LEFT,
  pattern GL_NEGATIVE_ONE_TO_ONE,
  pattern GL_UPPER_LEFT,
  pattern GL_ZERO_TO_ONE,
  -- * Functions
  glClipControl
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
