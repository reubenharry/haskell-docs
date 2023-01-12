{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.SeparateShaderObjects
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.SeparateShaderObjects (
  -- * Extension Support
  glGetEXTSeparateShaderObjects,
  gl_EXT_separate_shader_objects,
  -- * Enums
  pattern GL_ACTIVE_PROGRAM_EXT,
  -- * Functions
  glActiveProgramEXT,
  glCreateShaderProgramEXT,
  glUseShaderProgramEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
