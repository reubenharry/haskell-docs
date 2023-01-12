{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FragmentProgram2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FragmentProgram2 (
  -- * Extension Support
  glGetNVFragmentProgram2,
  gl_NV_fragment_program2,
  -- * Enums
  pattern GL_MAX_PROGRAM_CALL_DEPTH_NV,
  pattern GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV,
  pattern GL_MAX_PROGRAM_IF_DEPTH_NV,
  pattern GL_MAX_PROGRAM_LOOP_COUNT_NV,
  pattern GL_MAX_PROGRAM_LOOP_DEPTH_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
