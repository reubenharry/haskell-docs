{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SamplerObjects
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SamplerObjects (
  -- * Extension Support
  glGetARBSamplerObjects,
  gl_ARB_sampler_objects,
  -- * Enums
  pattern GL_SAMPLER_BINDING,
  -- * Functions
  glBindSampler,
  glDeleteSamplers,
  glGenSamplers,
  glGetSamplerParameterIiv,
  glGetSamplerParameterIuiv,
  glGetSamplerParameterfv,
  glGetSamplerParameteriv,
  glIsSampler,
  glSamplerParameterIiv,
  glSamplerParameterIuiv,
  glSamplerParameterf,
  glSamplerParameterfv,
  glSamplerParameteri,
  glSamplerParameteriv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
