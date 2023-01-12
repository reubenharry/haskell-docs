{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.PGI.VertexHints
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.PGI.VertexHints (
  -- * Extension Support
  glGetPGIVertexHints,
  gl_PGI_vertex_hints,
  -- * Enums
  pattern GL_COLOR3_BIT_PGI,
  pattern GL_COLOR4_BIT_PGI,
  pattern GL_EDGEFLAG_BIT_PGI,
  pattern GL_INDEX_BIT_PGI,
  pattern GL_MATERIAL_SIDE_HINT_PGI,
  pattern GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI,
  pattern GL_MAT_AMBIENT_BIT_PGI,
  pattern GL_MAT_COLOR_INDEXES_BIT_PGI,
  pattern GL_MAT_DIFFUSE_BIT_PGI,
  pattern GL_MAT_EMISSION_BIT_PGI,
  pattern GL_MAT_SHININESS_BIT_PGI,
  pattern GL_MAT_SPECULAR_BIT_PGI,
  pattern GL_MAX_VERTEX_HINT_PGI,
  pattern GL_NORMAL_BIT_PGI,
  pattern GL_TEXCOORD1_BIT_PGI,
  pattern GL_TEXCOORD2_BIT_PGI,
  pattern GL_TEXCOORD3_BIT_PGI,
  pattern GL_TEXCOORD4_BIT_PGI,
  pattern GL_VERTEX23_BIT_PGI,
  pattern GL_VERTEX4_BIT_PGI,
  pattern GL_VERTEX_CONSISTENT_HINT_PGI,
  pattern GL_VERTEX_DATA_HINT_PGI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
