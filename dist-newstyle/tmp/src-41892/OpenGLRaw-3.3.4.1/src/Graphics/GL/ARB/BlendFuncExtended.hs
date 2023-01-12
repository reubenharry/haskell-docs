{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.BlendFuncExtended
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.BlendFuncExtended (
  -- * Extension Support
  glGetARBBlendFuncExtended,
  gl_ARB_blend_func_extended,
  -- * Enums
  pattern GL_MAX_DUAL_SOURCE_DRAW_BUFFERS,
  pattern GL_ONE_MINUS_SRC1_ALPHA,
  pattern GL_ONE_MINUS_SRC1_COLOR,
  pattern GL_SRC1_ALPHA,
  pattern GL_SRC1_COLOR,
  -- * Functions
  glBindFragDataLocationIndexed,
  glGetFragDataIndex
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
