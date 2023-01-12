{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShadingLanguage100
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShadingLanguage100 (
  -- * Extension Support
  glGetARBShadingLanguage100,
  gl_ARB_shading_language_100,
  -- * Enums
  pattern GL_SHADING_LANGUAGE_VERSION_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
