{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.GlobalAlpha
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.GlobalAlpha (
  -- * Extension Support
  glGetSUNGlobalAlpha,
  gl_SUN_global_alpha,
  -- * Enums
  pattern GL_GLOBAL_ALPHA_FACTOR_SUN,
  pattern GL_GLOBAL_ALPHA_SUN,
  -- * Functions
  glGlobalAlphaFactorbSUN,
  glGlobalAlphaFactordSUN,
  glGlobalAlphaFactorfSUN,
  glGlobalAlphaFactoriSUN,
  glGlobalAlphaFactorsSUN,
  glGlobalAlphaFactorubSUN,
  glGlobalAlphaFactoruiSUN,
  glGlobalAlphaFactorusSUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
