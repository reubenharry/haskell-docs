{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.PolygonOffsetClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.PolygonOffsetClamp (
  -- * Extension Support
  glGetARBPolygonOffsetClamp,
  gl_ARB_polygon_offset_clamp,
  -- * Enums
  pattern GL_POLYGON_OFFSET_CLAMP,
  -- * Functions
  glPolygonOffsetClamp
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
