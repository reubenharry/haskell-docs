{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ViewportArray
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ViewportArray (
  -- * Extension Support
  glGetARBViewportArray,
  gl_ARB_viewport_array,
  -- * Enums
  pattern GL_DEPTH_RANGE,
  pattern GL_FIRST_VERTEX_CONVENTION,
  pattern GL_LAST_VERTEX_CONVENTION,
  pattern GL_LAYER_PROVOKING_VERTEX,
  pattern GL_MAX_VIEWPORTS,
  pattern GL_PROVOKING_VERTEX,
  pattern GL_SCISSOR_BOX,
  pattern GL_SCISSOR_TEST,
  pattern GL_UNDEFINED_VERTEX,
  pattern GL_VIEWPORT,
  pattern GL_VIEWPORT_BOUNDS_RANGE,
  pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX,
  pattern GL_VIEWPORT_SUBPIXEL_BITS,
  -- * Functions
  glDepthRangeArrayv,
  glDepthRangeIndexed,
  glGetDoublei_v,
  glGetFloati_v,
  glScissorArrayv,
  glScissorIndexed,
  glScissorIndexedv,
  glViewportArrayv,
  glViewportIndexedf,
  glViewportIndexedfv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
