{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.OcclusionQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.OcclusionQuery (
  -- * Extension Support
  glGetNVOcclusionQuery,
  gl_NV_occlusion_query,
  -- * Enums
  pattern GL_CURRENT_OCCLUSION_QUERY_ID_NV,
  pattern GL_PIXEL_COUNTER_BITS_NV,
  pattern GL_PIXEL_COUNT_AVAILABLE_NV,
  pattern GL_PIXEL_COUNT_NV,
  -- * Functions
  glBeginOcclusionQueryNV,
  glDeleteOcclusionQueriesNV,
  glEndOcclusionQueryNV,
  glGenOcclusionQueriesNV,
  glGetOcclusionQueryivNV,
  glGetOcclusionQueryuivNV,
  glIsOcclusionQueryNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
