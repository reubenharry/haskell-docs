{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.SeparateStencil
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.SeparateStencil (
  -- * Extension Support
  glGetATISeparateStencil,
  gl_ATI_separate_stencil,
  -- * Enums
  pattern GL_STENCIL_BACK_FAIL_ATI,
  pattern GL_STENCIL_BACK_FUNC_ATI,
  pattern GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI,
  pattern GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI,
  -- * Functions
  glStencilFuncSeparateATI,
  glStencilOpSeparateATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
