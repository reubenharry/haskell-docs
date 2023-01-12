--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DrawBuffersBlend
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DrawBuffersBlend (
  -- * Extension Support
  glGetARBDrawBuffersBlend,
  gl_ARB_draw_buffers_blend,
  -- * Functions
  glBlendEquationSeparateiARB,
  glBlendEquationiARB,
  glBlendFuncSeparateiARB,
  glBlendFunciARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
