{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.IBM.VertexArrayLists
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.IBM.VertexArrayLists (
  -- * Extension Support
  glGetIBMVertexArrayLists,
  gl_IBM_vertex_array_lists,
  -- * Enums
  pattern GL_COLOR_ARRAY_LIST_IBM,
  pattern GL_COLOR_ARRAY_LIST_STRIDE_IBM,
  pattern GL_EDGE_FLAG_ARRAY_LIST_IBM,
  pattern GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM,
  pattern GL_FOG_COORDINATE_ARRAY_LIST_IBM,
  pattern GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM,
  pattern GL_INDEX_ARRAY_LIST_IBM,
  pattern GL_INDEX_ARRAY_LIST_STRIDE_IBM,
  pattern GL_NORMAL_ARRAY_LIST_IBM,
  pattern GL_NORMAL_ARRAY_LIST_STRIDE_IBM,
  pattern GL_SECONDARY_COLOR_ARRAY_LIST_IBM,
  pattern GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM,
  pattern GL_TEXTURE_COORD_ARRAY_LIST_IBM,
  pattern GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM,
  pattern GL_VERTEX_ARRAY_LIST_IBM,
  pattern GL_VERTEX_ARRAY_LIST_STRIDE_IBM,
  -- * Functions
  glColorPointerListIBM,
  glEdgeFlagPointerListIBM,
  glFogCoordPointerListIBM,
  glIndexPointerListIBM,
  glNormalPointerListIBM,
  glSecondaryColorPointerListIBM,
  glTexCoordPointerListIBM,
  glVertexPointerListIBM
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
