{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.CompiledVertexArray
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.CompiledVertexArray (
  -- * Extension Support
  glGetEXTCompiledVertexArray,
  gl_EXT_compiled_vertex_array,
  -- * Enums
  pattern GL_ARRAY_ELEMENT_LOCK_COUNT_EXT,
  pattern GL_ARRAY_ELEMENT_LOCK_FIRST_EXT,
  -- * Functions
  glLockArraysEXT,
  glUnlockArraysEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
