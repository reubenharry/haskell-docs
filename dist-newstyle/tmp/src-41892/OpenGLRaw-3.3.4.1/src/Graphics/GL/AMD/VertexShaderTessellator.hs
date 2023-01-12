{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.VertexShaderTessellator
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.VertexShaderTessellator (
  -- * Extension Support
  glGetAMDVertexShaderTessellator,
  gl_AMD_vertex_shader_tessellator,
  -- * Enums
  pattern GL_CONTINUOUS_AMD,
  pattern GL_DISCRETE_AMD,
  pattern GL_INT_SAMPLER_BUFFER_AMD,
  pattern GL_SAMPLER_BUFFER_AMD,
  pattern GL_TESSELLATION_FACTOR_AMD,
  pattern GL_TESSELLATION_MODE_AMD,
  pattern GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD,
  -- * Functions
  glTessellationFactorAMD,
  glTessellationModeAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
