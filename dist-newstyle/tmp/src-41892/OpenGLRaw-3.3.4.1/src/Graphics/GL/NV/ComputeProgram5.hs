{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ComputeProgram5
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ComputeProgram5 (
  -- * Extension Support
  glGetNVComputeProgram5,
  gl_NV_compute_program5,
  -- * Enums
  pattern GL_COMPUTE_PROGRAM_NV,
  pattern GL_COMPUTE_PROGRAM_PARAMETER_BUFFER_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
