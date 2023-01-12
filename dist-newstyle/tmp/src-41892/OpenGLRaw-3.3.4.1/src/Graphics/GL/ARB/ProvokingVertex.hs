{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ProvokingVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ProvokingVertex (
  -- * Extension Support
  glGetARBProvokingVertex,
  gl_ARB_provoking_vertex,
  -- * Enums
  pattern GL_FIRST_VERTEX_CONVENTION,
  pattern GL_LAST_VERTEX_CONVENTION,
  pattern GL_PROVOKING_VERTEX,
  pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION,
  -- * Functions
  glProvokingVertex
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
