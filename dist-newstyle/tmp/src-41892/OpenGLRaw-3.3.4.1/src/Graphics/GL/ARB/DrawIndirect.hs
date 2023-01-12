{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DrawIndirect
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DrawIndirect (
  -- * Extension Support
  glGetARBDrawIndirect,
  gl_ARB_draw_indirect,
  -- * Enums
  pattern GL_DRAW_INDIRECT_BUFFER,
  pattern GL_DRAW_INDIRECT_BUFFER_BINDING,
  -- * Functions
  glDrawArraysIndirect,
  glDrawElementsIndirect
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
