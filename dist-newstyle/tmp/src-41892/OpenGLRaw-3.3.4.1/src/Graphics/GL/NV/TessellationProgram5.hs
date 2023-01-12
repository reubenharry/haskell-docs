{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TessellationProgram5
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TessellationProgram5 (
  -- * Extension Support
  glGetNVTessellationProgram5,
  gl_NV_tessellation_program5,
  -- * Enums
  pattern GL_MAX_PROGRAM_PATCH_ATTRIBS_NV,
  pattern GL_TESS_CONTROL_PROGRAM_NV,
  pattern GL_TESS_CONTROL_PROGRAM_PARAMETER_BUFFER_NV,
  pattern GL_TESS_EVALUATION_PROGRAM_NV,
  pattern GL_TESS_EVALUATION_PROGRAM_PARAMETER_BUFFER_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
