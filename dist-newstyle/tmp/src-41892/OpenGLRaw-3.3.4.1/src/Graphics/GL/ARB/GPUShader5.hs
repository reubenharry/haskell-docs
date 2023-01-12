{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GPUShader5
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GPUShader5 (
  -- * Extension Support
  glGetARBGPUShader5,
  gl_ARB_gpu_shader5,
  -- * Enums
  pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS,
  pattern GL_GEOMETRY_SHADER_INVOCATIONS,
  pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET,
  pattern GL_MAX_GEOMETRY_SHADER_INVOCATIONS,
  pattern GL_MAX_VERTEX_STREAMS,
  pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
