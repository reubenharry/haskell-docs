{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ProvokingVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ProvokingVertex (
  -- * Extension Support
  glGetEXTProvokingVertex,
  gl_EXT_provoking_vertex,
  -- * Enums
  pattern GL_FIRST_VERTEX_CONVENTION_EXT,
  pattern GL_LAST_VERTEX_CONVENTION_EXT,
  pattern GL_PROVOKING_VERTEX_EXT,
  pattern GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION_EXT,
  -- * Functions
  glProvokingVertexEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
