{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureMultisample (
  -- * Extension Support
  glGetARBTextureMultisample,
  gl_ARB_texture_multisample,
  -- * Enums
  pattern GL_INT_SAMPLER_2D_MULTISAMPLE,
  pattern GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
  pattern GL_MAX_COLOR_TEXTURE_SAMPLES,
  pattern GL_MAX_DEPTH_TEXTURE_SAMPLES,
  pattern GL_MAX_INTEGER_SAMPLES,
  pattern GL_MAX_SAMPLE_MASK_WORDS,
  pattern GL_PROXY_TEXTURE_2D_MULTISAMPLE,
  pattern GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY,
  pattern GL_SAMPLER_2D_MULTISAMPLE,
  pattern GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
  pattern GL_SAMPLE_MASK,
  pattern GL_SAMPLE_MASK_VALUE,
  pattern GL_SAMPLE_POSITION,
  pattern GL_TEXTURE_2D_MULTISAMPLE,
  pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
  pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE,
  pattern GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY,
  pattern GL_TEXTURE_FIXED_SAMPLE_LOCATIONS,
  pattern GL_TEXTURE_SAMPLES,
  pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE,
  pattern GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
  -- * Functions
  glGetMultisamplefv,
  glSampleMaski,
  glTexImage2DMultisample,
  glTexImage3DMultisample
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
