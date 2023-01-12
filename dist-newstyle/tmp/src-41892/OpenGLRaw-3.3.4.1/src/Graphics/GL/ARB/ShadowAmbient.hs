{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShadowAmbient
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShadowAmbient (
  -- * Extension Support
  glGetARBShadowAmbient,
  gl_ARB_shadow_ambient,
  -- * Enums
  pattern GL_TEXTURE_COMPARE_FAIL_VALUE_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
