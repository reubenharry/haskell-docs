--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.BindlessMultiDrawIndirect
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.BindlessMultiDrawIndirect (
  -- * Extension Support
  glGetNVBindlessMultiDrawIndirect,
  gl_NV_bindless_multi_draw_indirect,
  -- * Functions
  glMultiDrawArraysIndirectBindlessNV,
  glMultiDrawElementsIndirectBindlessNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
