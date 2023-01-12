{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.InterleavedElements
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.InterleavedElements (
  -- * Extension Support
  glGetAMDInterleavedElements,
  gl_AMD_interleaved_elements,
  -- * Enums
  pattern GL_ALPHA,
  pattern GL_BLUE,
  pattern GL_GREEN,
  pattern GL_RED,
  pattern GL_RG16UI,
  pattern GL_RG8UI,
  pattern GL_RGBA8UI,
  pattern GL_VERTEX_ELEMENT_SWIZZLE_AMD,
  pattern GL_VERTEX_ID_SWIZZLE_AMD,
  -- * Functions
  glVertexAttribParameteriAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
