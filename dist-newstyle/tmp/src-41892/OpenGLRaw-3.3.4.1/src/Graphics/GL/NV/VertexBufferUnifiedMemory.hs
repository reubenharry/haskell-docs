{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexBufferUnifiedMemory
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexBufferUnifiedMemory (
  -- * Extension Support
  glGetNVVertexBufferUnifiedMemory,
  gl_NV_vertex_buffer_unified_memory,
  -- * Enums
  pattern GL_COLOR_ARRAY_ADDRESS_NV,
  pattern GL_COLOR_ARRAY_LENGTH_NV,
  pattern GL_DRAW_INDIRECT_ADDRESS_NV,
  pattern GL_DRAW_INDIRECT_LENGTH_NV,
  pattern GL_DRAW_INDIRECT_UNIFIED_NV,
  pattern GL_EDGE_FLAG_ARRAY_ADDRESS_NV,
  pattern GL_EDGE_FLAG_ARRAY_LENGTH_NV,
  pattern GL_ELEMENT_ARRAY_ADDRESS_NV,
  pattern GL_ELEMENT_ARRAY_LENGTH_NV,
  pattern GL_ELEMENT_ARRAY_UNIFIED_NV,
  pattern GL_FOG_COORD_ARRAY_ADDRESS_NV,
  pattern GL_FOG_COORD_ARRAY_LENGTH_NV,
  pattern GL_INDEX_ARRAY_ADDRESS_NV,
  pattern GL_INDEX_ARRAY_LENGTH_NV,
  pattern GL_NORMAL_ARRAY_ADDRESS_NV,
  pattern GL_NORMAL_ARRAY_LENGTH_NV,
  pattern GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV,
  pattern GL_SECONDARY_COLOR_ARRAY_LENGTH_NV,
  pattern GL_TEXTURE_COORD_ARRAY_ADDRESS_NV,
  pattern GL_TEXTURE_COORD_ARRAY_LENGTH_NV,
  pattern GL_VERTEX_ARRAY_ADDRESS_NV,
  pattern GL_VERTEX_ARRAY_LENGTH_NV,
  pattern GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV,
  pattern GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV,
  pattern GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV,
  -- * Functions
  glBufferAddressRangeNV,
  glColorFormatNV,
  glEdgeFlagFormatNV,
  glFogCoordFormatNV,
  glGetIntegerui64i_vNV,
  glIndexFormatNV,
  glNormalFormatNV,
  glSecondaryColorFormatNV,
  glTexCoordFormatNV,
  glVertexAttribFormatNV,
  glVertexAttribIFormatNV,
  glVertexFormatNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
