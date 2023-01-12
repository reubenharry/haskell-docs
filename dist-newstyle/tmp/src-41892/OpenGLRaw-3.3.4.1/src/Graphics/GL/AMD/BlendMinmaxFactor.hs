{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.BlendMinmaxFactor
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.BlendMinmaxFactor (
  -- * Extension Support
  glGetAMDBlendMinmaxFactor,
  gl_AMD_blend_minmax_factor,
  -- * Enums
  pattern GL_FACTOR_MAX_AMD,
  pattern GL_FACTOR_MIN_AMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
