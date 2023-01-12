{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ExplicitMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ExplicitMultisample (
  -- * Extension Support
  glGetNVExplicitMultisample,
  gl_NV_explicit_multisample,
  -- * Enums
  pattern GL_INT_SAMPLER_RENDERBUFFER_NV,
  pattern GL_MAX_SAMPLE_MASK_WORDS_NV,
  pattern GL_SAMPLER_RENDERBUFFER_NV,
  pattern GL_SAMPLE_MASK_NV,
  pattern GL_SAMPLE_MASK_VALUE_NV,
  pattern GL_SAMPLE_POSITION_NV,
  pattern GL_TEXTURE_BINDING_RENDERBUFFER_NV,
  pattern GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV,
  pattern GL_TEXTURE_RENDERBUFFER_NV,
  pattern GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV,
  -- * Functions
  glGetMultisamplefvNV,
  glSampleMaskIndexedNV,
  glTexRenderbufferNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
