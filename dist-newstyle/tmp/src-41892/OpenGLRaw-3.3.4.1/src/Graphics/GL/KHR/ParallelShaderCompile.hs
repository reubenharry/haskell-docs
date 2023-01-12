{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.ParallelShaderCompile
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.ParallelShaderCompile (
  -- * Extension Support
  glGetKHRParallelShaderCompile,
  gl_KHR_parallel_shader_compile,
  -- * Enums
  pattern GL_COMPLETION_STATUS_KHR,
  pattern GL_MAX_SHADER_COMPILER_THREADS_KHR,
  -- * Functions
  glMaxShaderCompilerThreadsKHR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
