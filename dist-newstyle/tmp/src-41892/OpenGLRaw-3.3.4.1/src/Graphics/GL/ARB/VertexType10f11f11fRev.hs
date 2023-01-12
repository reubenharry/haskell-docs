{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexType10f11f11fRev
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexType10f11f11fRev (
  -- * Extension Support
  glGetARBVertexType10f11f11fRev,
  gl_ARB_vertex_type_10f_11f_11f_rev,
  -- * Enums
  pattern GL_UNSIGNED_INT_10F_11F_11F_REV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
