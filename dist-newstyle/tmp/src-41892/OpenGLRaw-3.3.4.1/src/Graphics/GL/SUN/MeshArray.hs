{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.MeshArray
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.MeshArray (
  -- * Extension Support
  glGetSUNMeshArray,
  gl_SUN_mesh_array,
  -- * Enums
  pattern GL_QUAD_MESH_SUN,
  pattern GL_TRIANGLE_MESH_SUN,
  -- * Functions
  glDrawMeshArraysSUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
