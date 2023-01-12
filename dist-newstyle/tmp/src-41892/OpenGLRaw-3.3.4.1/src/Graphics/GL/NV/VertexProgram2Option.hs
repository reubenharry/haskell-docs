{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexProgram2Option
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexProgram2Option (
  -- * Extension Support
  glGetNVVertexProgram2Option,
  gl_NV_vertex_program2_option,
  -- * Enums
  pattern GL_MAX_PROGRAM_CALL_DEPTH_NV,
  pattern GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
