{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.FragmentShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.FragmentShader (
  -- * Extension Support
  glGetARBFragmentShader,
  gl_ARB_fragment_shader,
  -- * Enums
  pattern GL_FRAGMENT_SHADER_ARB,
  pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB,
  pattern GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
