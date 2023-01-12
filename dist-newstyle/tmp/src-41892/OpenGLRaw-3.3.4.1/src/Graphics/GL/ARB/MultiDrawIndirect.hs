--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.MultiDrawIndirect
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.MultiDrawIndirect (
  -- * Extension Support
  glGetARBMultiDrawIndirect,
  gl_ARB_multi_draw_indirect,
  -- * Functions
  glMultiDrawArraysIndirect,
  glMultiDrawElementsIndirect
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
