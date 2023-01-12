{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexArrayRange2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexArrayRange2 (
  -- * Extension Support
  glGetNVVertexArrayRange2,
  gl_NV_vertex_array_range2,
  -- * Enums
  pattern GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
