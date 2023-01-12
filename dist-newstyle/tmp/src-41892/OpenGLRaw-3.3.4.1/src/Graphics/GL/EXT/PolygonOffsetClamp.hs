{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PolygonOffsetClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PolygonOffsetClamp (
  -- * Extension Support
  glGetEXTPolygonOffsetClamp,
  gl_EXT_polygon_offset_clamp,
  -- * Enums
  pattern GL_POLYGON_OFFSET_CLAMP_EXT,
  -- * Functions
  glPolygonOffsetClampEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
