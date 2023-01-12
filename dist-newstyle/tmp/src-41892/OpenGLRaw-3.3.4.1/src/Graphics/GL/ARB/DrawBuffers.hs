{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DrawBuffers
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DrawBuffers (
  -- * Extension Support
  glGetARBDrawBuffers,
  gl_ARB_draw_buffers,
  -- * Enums
  pattern GL_DRAW_BUFFER0_ARB,
  pattern GL_DRAW_BUFFER10_ARB,
  pattern GL_DRAW_BUFFER11_ARB,
  pattern GL_DRAW_BUFFER12_ARB,
  pattern GL_DRAW_BUFFER13_ARB,
  pattern GL_DRAW_BUFFER14_ARB,
  pattern GL_DRAW_BUFFER15_ARB,
  pattern GL_DRAW_BUFFER1_ARB,
  pattern GL_DRAW_BUFFER2_ARB,
  pattern GL_DRAW_BUFFER3_ARB,
  pattern GL_DRAW_BUFFER4_ARB,
  pattern GL_DRAW_BUFFER5_ARB,
  pattern GL_DRAW_BUFFER6_ARB,
  pattern GL_DRAW_BUFFER7_ARB,
  pattern GL_DRAW_BUFFER8_ARB,
  pattern GL_DRAW_BUFFER9_ARB,
  pattern GL_MAX_DRAW_BUFFERS_ARB,
  -- * Functions
  glDrawBuffersARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
