--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DrawBuffers2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DrawBuffers2 (
  -- * Extension Support
  glGetEXTDrawBuffers2,
  gl_EXT_draw_buffers2,
  -- * Functions
  glColorMaskIndexedEXT,
  glDisableIndexedEXT,
  glEnableIndexedEXT,
  glGetBooleanIndexedvEXT,
  glGetIntegerIndexedvEXT,
  glIsEnabledIndexedEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
