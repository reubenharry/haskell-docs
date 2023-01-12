{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.IBM.CullVertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.IBM.CullVertex (
  -- * Extension Support
  glGetIBMCullVertex,
  gl_IBM_cull_vertex,
  -- * Enums
  pattern GL_CULL_VERTEX_IBM
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
