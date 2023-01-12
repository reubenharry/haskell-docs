{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ParallelShaderCompile
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ParallelShaderCompile (
  -- * Extension Support
  glGetARBParallelShaderCompile,
  gl_ARB_parallel_shader_compile,
  -- * Enums
  pattern GL_COMPLETION_STATUS_ARB,
  pattern GL_MAX_SHADER_COMPILER_THREADS_ARB,
  -- * Functions
  glMaxShaderCompilerThreadsARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
