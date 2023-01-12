--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DrawInstanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DrawInstanced (
  -- * Extension Support
  glGetARBDrawInstanced,
  gl_ARB_draw_instanced,
  -- * Functions
  glDrawArraysInstancedARB,
  glDrawElementsInstancedARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
