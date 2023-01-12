{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.LightMaxExponent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.LightMaxExponent (
  -- * Extension Support
  glGetNVLightMaxExponent,
  gl_NV_light_max_exponent,
  -- * Enums
  pattern GL_MAX_SHININESS_NV,
  pattern GL_MAX_SPOT_EXPONENT_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
