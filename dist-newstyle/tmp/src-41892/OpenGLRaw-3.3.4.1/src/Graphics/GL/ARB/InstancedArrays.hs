{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.InstancedArrays
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.InstancedArrays (
  -- * Extension Support
  glGetARBInstancedArrays,
  gl_ARB_instanced_arrays,
  -- * Enums
  pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB,
  -- * Functions
  glVertexAttribDivisorARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
