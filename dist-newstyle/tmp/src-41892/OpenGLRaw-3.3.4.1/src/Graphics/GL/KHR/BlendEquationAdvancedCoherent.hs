{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.BlendEquationAdvancedCoherent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.BlendEquationAdvancedCoherent (
  -- * Extension Support
  glGetKHRBlendEquationAdvancedCoherent,
  gl_KHR_blend_equation_advanced_coherent,
  -- * Enums
  pattern GL_BLEND_ADVANCED_COHERENT_KHR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
