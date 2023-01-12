--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INGR.BlendFuncSeparate
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INGR.BlendFuncSeparate (
  -- * Extension Support
  glGetINGRBlendFuncSeparate,
  gl_INGR_blend_func_separate,
  -- * Functions
  glBlendFuncSeparateINGR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
