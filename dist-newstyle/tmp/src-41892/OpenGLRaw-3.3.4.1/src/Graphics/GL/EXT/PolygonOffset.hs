{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PolygonOffset
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PolygonOffset (
  -- * Extension Support
  glGetEXTPolygonOffset,
  gl_EXT_polygon_offset,
  -- * Enums
  pattern GL_POLYGON_OFFSET_BIAS_EXT,
  pattern GL_POLYGON_OFFSET_EXT,
  pattern GL_POLYGON_OFFSET_FACTOR_EXT,
  -- * Functions
  glPolygonOffsetEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
