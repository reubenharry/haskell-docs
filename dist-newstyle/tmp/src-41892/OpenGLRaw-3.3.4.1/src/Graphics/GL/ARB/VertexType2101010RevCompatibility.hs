{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexType2101010RevCompatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexType2101010RevCompatibility (
  -- * Extension Support
  glGetARBVertexType2101010Rev,
  gl_ARB_vertex_type_2_10_10_10_rev,
  -- * Enums
  pattern GL_INT_2_10_10_10_REV,
  pattern GL_UNSIGNED_INT_2_10_10_10_REV,
  -- * Functions
  glColorP3ui,
  glColorP3uiv,
  glColorP4ui,
  glColorP4uiv,
  glMultiTexCoordP1ui,
  glMultiTexCoordP1uiv,
  glMultiTexCoordP2ui,
  glMultiTexCoordP2uiv,
  glMultiTexCoordP3ui,
  glMultiTexCoordP3uiv,
  glMultiTexCoordP4ui,
  glMultiTexCoordP4uiv,
  glNormalP3ui,
  glNormalP3uiv,
  glSecondaryColorP3ui,
  glSecondaryColorP3uiv,
  glTexCoordP1ui,
  glTexCoordP1uiv,
  glTexCoordP2ui,
  glTexCoordP2uiv,
  glTexCoordP3ui,
  glTexCoordP3uiv,
  glTexCoordP4ui,
  glTexCoordP4uiv,
  glVertexAttribP1ui,
  glVertexAttribP1uiv,
  glVertexAttribP2ui,
  glVertexAttribP2uiv,
  glVertexAttribP3ui,
  glVertexAttribP3uiv,
  glVertexAttribP4ui,
  glVertexAttribP4uiv,
  glVertexP2ui,
  glVertexP2uiv,
  glVertexP3ui,
  glVertexP3uiv,
  glVertexP4ui,
  glVertexP4uiv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
