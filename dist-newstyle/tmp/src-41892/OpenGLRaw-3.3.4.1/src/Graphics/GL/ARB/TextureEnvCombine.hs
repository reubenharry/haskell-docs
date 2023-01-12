{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureEnvCombine
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureEnvCombine (
  -- * Extension Support
  glGetARBTextureEnvCombine,
  gl_ARB_texture_env_combine,
  -- * Enums
  pattern GL_ADD_SIGNED_ARB,
  pattern GL_COMBINE_ALPHA_ARB,
  pattern GL_COMBINE_ARB,
  pattern GL_COMBINE_RGB_ARB,
  pattern GL_CONSTANT_ARB,
  pattern GL_INTERPOLATE_ARB,
  pattern GL_OPERAND0_ALPHA_ARB,
  pattern GL_OPERAND0_RGB_ARB,
  pattern GL_OPERAND1_ALPHA_ARB,
  pattern GL_OPERAND1_RGB_ARB,
  pattern GL_OPERAND2_ALPHA_ARB,
  pattern GL_OPERAND2_RGB_ARB,
  pattern GL_PREVIOUS_ARB,
  pattern GL_PRIMARY_COLOR_ARB,
  pattern GL_RGB_SCALE_ARB,
  pattern GL_SOURCE0_ALPHA_ARB,
  pattern GL_SOURCE0_RGB_ARB,
  pattern GL_SOURCE1_ALPHA_ARB,
  pattern GL_SOURCE1_RGB_ARB,
  pattern GL_SOURCE2_ALPHA_ARB,
  pattern GL_SOURCE2_RGB_ARB,
  pattern GL_SUBTRACT_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
