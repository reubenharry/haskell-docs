{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.CullVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.CullVertex (
  -- * Extension Support
  glGetEXTCullVertex,
  gl_EXT_cull_vertex,
  -- * Enums
  pattern GL_CULL_VERTEX_EXT,
  pattern GL_CULL_VERTEX_EYE_POSITION_EXT,
  pattern GL_CULL_VERTEX_OBJECT_POSITION_EXT,
  -- * Functions
  glCullParameterdvEXT,
  glCullParameterfvEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
