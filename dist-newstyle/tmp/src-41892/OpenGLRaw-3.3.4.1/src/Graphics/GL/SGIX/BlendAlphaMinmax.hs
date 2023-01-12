{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.BlendAlphaMinmax
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.BlendAlphaMinmax (
  -- * Extension Support
  glGetSGIXBlendAlphaMinmax,
  gl_SGIX_blend_alpha_minmax,
  -- * Enums
  pattern GL_ALPHA_MAX_SGIX,
  pattern GL_ALPHA_MIN_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
