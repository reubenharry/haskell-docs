{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.Multisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.Multisample (
  -- * Extension Support
  glGetSGISMultisample,
  gl_SGIS_multisample,
  -- * Enums
  pattern GL_1PASS_SGIS,
  pattern GL_2PASS_0_SGIS,
  pattern GL_2PASS_1_SGIS,
  pattern GL_4PASS_0_SGIS,
  pattern GL_4PASS_1_SGIS,
  pattern GL_4PASS_2_SGIS,
  pattern GL_4PASS_3_SGIS,
  pattern GL_MULTISAMPLE_SGIS,
  pattern GL_SAMPLES_SGIS,
  pattern GL_SAMPLE_ALPHA_TO_MASK_SGIS,
  pattern GL_SAMPLE_ALPHA_TO_ONE_SGIS,
  pattern GL_SAMPLE_BUFFERS_SGIS,
  pattern GL_SAMPLE_MASK_INVERT_SGIS,
  pattern GL_SAMPLE_MASK_SGIS,
  pattern GL_SAMPLE_MASK_VALUE_SGIS,
  pattern GL_SAMPLE_PATTERN_SGIS,
  -- * Functions
  glSampleMaskSGIS,
  glSamplePatternSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
