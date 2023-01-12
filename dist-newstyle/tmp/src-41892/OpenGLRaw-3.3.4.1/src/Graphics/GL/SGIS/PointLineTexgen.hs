{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.PointLineTexgen
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.PointLineTexgen (
  -- * Extension Support
  glGetSGISPointLineTexgen,
  gl_SGIS_point_line_texgen,
  -- * Enums
  pattern GL_EYE_DISTANCE_TO_LINE_SGIS,
  pattern GL_EYE_DISTANCE_TO_POINT_SGIS,
  pattern GL_EYE_LINE_SGIS,
  pattern GL_EYE_POINT_SGIS,
  pattern GL_OBJECT_DISTANCE_TO_LINE_SGIS,
  pattern GL_OBJECT_DISTANCE_TO_POINT_SGIS,
  pattern GL_OBJECT_LINE_SGIS,
  pattern GL_OBJECT_POINT_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
