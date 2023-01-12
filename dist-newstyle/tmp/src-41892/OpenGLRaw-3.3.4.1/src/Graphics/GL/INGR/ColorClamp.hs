{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INGR.ColorClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INGR.ColorClamp (
  -- * Extension Support
  glGetINGRColorClamp,
  gl_INGR_color_clamp,
  -- * Enums
  pattern GL_ALPHA_MAX_CLAMP_INGR,
  pattern GL_ALPHA_MIN_CLAMP_INGR,
  pattern GL_BLUE_MAX_CLAMP_INGR,
  pattern GL_BLUE_MIN_CLAMP_INGR,
  pattern GL_GREEN_MAX_CLAMP_INGR,
  pattern GL_GREEN_MIN_CLAMP_INGR,
  pattern GL_RED_MAX_CLAMP_INGR,
  pattern GL_RED_MIN_CLAMP_INGR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
