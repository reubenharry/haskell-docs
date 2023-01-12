{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureEnvCombine
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureEnvCombine (
  -- * Extension Support
  glGetEXTTextureEnvCombine,
  gl_EXT_texture_env_combine,
  -- * Enums
  pattern GL_ADD_SIGNED_EXT,
  pattern GL_COMBINE_ALPHA_EXT,
  pattern GL_COMBINE_EXT,
  pattern GL_COMBINE_RGB_EXT,
  pattern GL_CONSTANT_EXT,
  pattern GL_INTERPOLATE_EXT,
  pattern GL_OPERAND0_ALPHA_EXT,
  pattern GL_OPERAND0_RGB_EXT,
  pattern GL_OPERAND1_ALPHA_EXT,
  pattern GL_OPERAND1_RGB_EXT,
  pattern GL_OPERAND2_ALPHA_EXT,
  pattern GL_OPERAND2_RGB_EXT,
  pattern GL_PREVIOUS_EXT,
  pattern GL_PRIMARY_COLOR_EXT,
  pattern GL_RGB_SCALE_EXT,
  pattern GL_SOURCE0_ALPHA_EXT,
  pattern GL_SOURCE0_RGB_EXT,
  pattern GL_SOURCE1_ALPHA_EXT,
  pattern GL_SOURCE1_RGB_EXT,
  pattern GL_SOURCE2_ALPHA_EXT,
  pattern GL_SOURCE2_RGB_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
