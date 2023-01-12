{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESA.ProgramBinaryFormats
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESA.ProgramBinaryFormats (
  -- * Extension Support
  glGetMESAProgramBinaryFormats,
  gl_MESA_program_binary_formats,
  -- * Enums
  pattern GL_PROGRAM_BINARY_FORMAT_MESA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
