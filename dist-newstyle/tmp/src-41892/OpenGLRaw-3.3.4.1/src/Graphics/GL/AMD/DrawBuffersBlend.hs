--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.DrawBuffersBlend
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.DrawBuffersBlend (
  -- * Extension Support
  glGetAMDDrawBuffersBlend,
  gl_AMD_draw_buffers_blend,
  -- * Functions
  glBlendEquationIndexedAMD,
  glBlendEquationSeparateIndexedAMD,
  glBlendFuncIndexedAMD,
  glBlendFuncSeparateIndexedAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
