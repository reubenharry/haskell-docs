--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.MultiDrawIndirect
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.MultiDrawIndirect (
  -- * Extension Support
  glGetAMDMultiDrawIndirect,
  gl_AMD_multi_draw_indirect,
  -- * Functions
  glMultiDrawArraysIndirectAMD,
  glMultiDrawElementsIndirectAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
