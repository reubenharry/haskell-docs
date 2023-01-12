{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ThreeDFX.Multisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ThreeDFX.Multisample (
  -- * Extension Support
  glGetThreeDFXMultisample,
  gl_3DFX_multisample,
  -- * Enums
  pattern GL_MULTISAMPLE_3DFX,
  pattern GL_MULTISAMPLE_BIT_3DFX,
  pattern GL_SAMPLES_3DFX,
  pattern GL_SAMPLE_BUFFERS_3DFX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
