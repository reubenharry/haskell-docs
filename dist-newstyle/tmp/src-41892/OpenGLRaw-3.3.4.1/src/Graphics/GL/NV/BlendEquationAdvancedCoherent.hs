{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.BlendEquationAdvancedCoherent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.BlendEquationAdvancedCoherent (
  -- * Extension Support
  glGetNVBlendEquationAdvancedCoherent,
  gl_NV_blend_equation_advanced_coherent,
  -- * Enums
  pattern GL_BLEND_ADVANCED_COHERENT_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
