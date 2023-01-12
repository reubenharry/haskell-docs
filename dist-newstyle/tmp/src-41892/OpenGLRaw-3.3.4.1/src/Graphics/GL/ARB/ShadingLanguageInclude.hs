{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShadingLanguageInclude
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShadingLanguageInclude (
  -- * Extension Support
  glGetARBShadingLanguageInclude,
  gl_ARB_shading_language_include,
  -- * Enums
  pattern GL_NAMED_STRING_LENGTH_ARB,
  pattern GL_NAMED_STRING_TYPE_ARB,
  pattern GL_SHADER_INCLUDE_ARB,
  -- * Functions
  glCompileShaderIncludeARB,
  glDeleteNamedStringARB,
  glGetNamedStringARB,
  glGetNamedStringivARB,
  glIsNamedStringARB,
  glNamedStringARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
