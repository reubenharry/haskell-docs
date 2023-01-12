{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexArrayBGRA
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexArrayBGRA (
  -- * Extension Support
  glGetARBVertexArrayBGRA,
  gl_ARB_vertex_array_bgra,
  -- * Enums
  pattern GL_BGRA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
