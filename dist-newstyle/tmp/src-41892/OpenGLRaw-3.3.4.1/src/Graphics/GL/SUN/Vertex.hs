--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.Vertex
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.Vertex (
  -- * Extension Support
  glGetSUNVertex,
  gl_SUN_vertex,
  -- * Functions
  glColor3fVertex3fSUN,
  glColor3fVertex3fvSUN,
  glColor4fNormal3fVertex3fSUN,
  glColor4fNormal3fVertex3fvSUN,
  glColor4ubVertex2fSUN,
  glColor4ubVertex2fvSUN,
  glColor4ubVertex3fSUN,
  glColor4ubVertex3fvSUN,
  glNormal3fVertex3fSUN,
  glNormal3fVertex3fvSUN,
  glReplacementCodeuiColor3fVertex3fSUN,
  glReplacementCodeuiColor3fVertex3fvSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiColor4ubVertex3fSUN,
  glReplacementCodeuiColor4ubVertex3fvSUN,
  glReplacementCodeuiNormal3fVertex3fSUN,
  glReplacementCodeuiNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fVertex3fSUN,
  glReplacementCodeuiTexCoord2fVertex3fvSUN,
  glReplacementCodeuiVertex3fSUN,
  glReplacementCodeuiVertex3fvSUN,
  glTexCoord2fColor3fVertex3fSUN,
  glTexCoord2fColor3fVertex3fvSUN,
  glTexCoord2fColor4fNormal3fVertex3fSUN,
  glTexCoord2fColor4fNormal3fVertex3fvSUN,
  glTexCoord2fColor4ubVertex3fSUN,
  glTexCoord2fColor4ubVertex3fvSUN,
  glTexCoord2fNormal3fVertex3fSUN,
  glTexCoord2fNormal3fVertex3fvSUN,
  glTexCoord2fVertex3fSUN,
  glTexCoord2fVertex3fvSUN,
  glTexCoord4fColor4fNormal3fVertex4fSUN,
  glTexCoord4fColor4fNormal3fVertex4fvSUN,
  glTexCoord4fVertex4fSUN,
  glTexCoord4fVertex4fvSUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
