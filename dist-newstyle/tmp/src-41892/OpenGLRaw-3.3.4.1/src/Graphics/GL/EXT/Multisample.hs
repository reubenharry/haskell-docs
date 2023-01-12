{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.Multisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.Multisample (
  -- * Extension Support
  glGetEXTMultisample,
  gl_EXT_multisample,
  -- * Enums
  pattern GL_1PASS_EXT,
  pattern GL_2PASS_0_EXT,
  pattern GL_2PASS_1_EXT,
  pattern GL_4PASS_0_EXT,
  pattern GL_4PASS_1_EXT,
  pattern GL_4PASS_2_EXT,
  pattern GL_4PASS_3_EXT,
  pattern GL_MULTISAMPLE_BIT_EXT,
  pattern GL_MULTISAMPLE_EXT,
  pattern GL_SAMPLES_EXT,
  pattern GL_SAMPLE_ALPHA_TO_MASK_EXT,
  pattern GL_SAMPLE_ALPHA_TO_ONE_EXT,
  pattern GL_SAMPLE_BUFFERS_EXT,
  pattern GL_SAMPLE_MASK_EXT,
  pattern GL_SAMPLE_MASK_INVERT_EXT,
  pattern GL_SAMPLE_MASK_VALUE_EXT,
  pattern GL_SAMPLE_PATTERN_EXT,
  -- * Functions
  glSampleMaskEXT,
  glSamplePatternEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
