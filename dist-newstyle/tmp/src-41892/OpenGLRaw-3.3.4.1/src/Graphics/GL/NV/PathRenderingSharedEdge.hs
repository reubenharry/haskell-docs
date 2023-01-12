{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.PathRenderingSharedEdge
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.PathRenderingSharedEdge (
  -- * Extension Support
  glGetNVPathRenderingSharedEdge,
  gl_NV_path_rendering_shared_edge,
  -- * Enums
  pattern GL_SHARED_EDGE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
