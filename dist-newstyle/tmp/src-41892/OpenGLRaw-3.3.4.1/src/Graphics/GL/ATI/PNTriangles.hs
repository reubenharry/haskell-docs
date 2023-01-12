{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.PNTriangles
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.PNTriangles (
  -- * Extension Support
  glGetATIPNTriangles,
  gl_ATI_pn_triangles,
  -- * Enums
  pattern GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI,
  pattern GL_PN_TRIANGLES_ATI,
  pattern GL_PN_TRIANGLES_NORMAL_MODE_ATI,
  pattern GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI,
  pattern GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI,
  pattern GL_PN_TRIANGLES_POINT_MODE_ATI,
  pattern GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI,
  pattern GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI,
  pattern GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI,
  -- * Functions
  glPNTrianglesfATI,
  glPNTrianglesiATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
