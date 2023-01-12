{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.DrawBuffers
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.DrawBuffers (
  -- * Extension Support
  glGetATIDrawBuffers,
  gl_ATI_draw_buffers,
  -- * Enums
  pattern GL_DRAW_BUFFER0_ATI,
  pattern GL_DRAW_BUFFER10_ATI,
  pattern GL_DRAW_BUFFER11_ATI,
  pattern GL_DRAW_BUFFER12_ATI,
  pattern GL_DRAW_BUFFER13_ATI,
  pattern GL_DRAW_BUFFER14_ATI,
  pattern GL_DRAW_BUFFER15_ATI,
  pattern GL_DRAW_BUFFER1_ATI,
  pattern GL_DRAW_BUFFER2_ATI,
  pattern GL_DRAW_BUFFER3_ATI,
  pattern GL_DRAW_BUFFER4_ATI,
  pattern GL_DRAW_BUFFER5_ATI,
  pattern GL_DRAW_BUFFER6_ATI,
  pattern GL_DRAW_BUFFER7_ATI,
  pattern GL_DRAW_BUFFER8_ATI,
  pattern GL_DRAW_BUFFER9_ATI,
  pattern GL_MAX_DRAW_BUFFERS_ATI,
  -- * Functions
  glDrawBuffersATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
