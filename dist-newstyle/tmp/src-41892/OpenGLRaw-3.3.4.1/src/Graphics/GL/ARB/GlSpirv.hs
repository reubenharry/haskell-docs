{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GlSpirv
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GlSpirv (
  -- * Extension Support
  glGetARBGlSpirv,
  gl_ARB_gl_spirv,
  -- * Enums
  pattern GL_SHADER_BINARY_FORMAT_SPIR_V_ARB,
  pattern GL_SPIR_V_BINARY_ARB,
  -- * Functions
  glSpecializeShaderARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
