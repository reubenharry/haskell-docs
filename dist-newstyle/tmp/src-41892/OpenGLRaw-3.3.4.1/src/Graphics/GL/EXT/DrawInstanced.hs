--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DrawInstanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DrawInstanced (
  -- * Extension Support
  glGetEXTDrawInstanced,
  gl_EXT_draw_instanced,
  -- * Functions
  glDrawArraysInstancedEXT,
  glDrawElementsInstancedEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
