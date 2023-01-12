--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.BindlessMultiDrawIndirectCount
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.BindlessMultiDrawIndirectCount (
  -- * Extension Support
  glGetNVBindlessMultiDrawIndirectCount,
  gl_NV_bindless_multi_draw_indirect_count,
  -- * Functions
  glMultiDrawArraysIndirectBindlessCountNV,
  glMultiDrawElementsIndirectBindlessCountNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
