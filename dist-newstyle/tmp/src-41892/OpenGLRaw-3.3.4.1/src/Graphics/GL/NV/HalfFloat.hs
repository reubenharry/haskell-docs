{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.HalfFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.HalfFloat (
  -- * Extension Support
  glGetNVHalfFloat,
  gl_NV_half_float,
  -- * Enums
  pattern GL_HALF_FLOAT_NV,
  -- * Functions
  glColor3hNV,
  glColor3hvNV,
  glColor4hNV,
  glColor4hvNV,
  glFogCoordhNV,
  glFogCoordhvNV,
  glMultiTexCoord1hNV,
  glMultiTexCoord1hvNV,
  glMultiTexCoord2hNV,
  glMultiTexCoord2hvNV,
  glMultiTexCoord3hNV,
  glMultiTexCoord3hvNV,
  glMultiTexCoord4hNV,
  glMultiTexCoord4hvNV,
  glNormal3hNV,
  glNormal3hvNV,
  glSecondaryColor3hNV,
  glSecondaryColor3hvNV,
  glTexCoord1hNV,
  glTexCoord1hvNV,
  glTexCoord2hNV,
  glTexCoord2hvNV,
  glTexCoord3hNV,
  glTexCoord3hvNV,
  glTexCoord4hNV,
  glTexCoord4hvNV,
  glVertex2hNV,
  glVertex2hvNV,
  glVertex3hNV,
  glVertex3hvNV,
  glVertex4hNV,
  glVertex4hvNV,
  glVertexAttrib1hNV,
  glVertexAttrib1hvNV,
  glVertexAttrib2hNV,
  glVertexAttrib2hvNV,
  glVertexAttrib3hNV,
  glVertexAttrib3hvNV,
  glVertexAttrib4hNV,
  glVertexAttrib4hvNV,
  glVertexAttribs1hvNV,
  glVertexAttribs2hvNV,
  glVertexAttribs3hvNV,
  glVertexAttribs4hvNV,
  glVertexWeighthNV,
  glVertexWeighthvNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
