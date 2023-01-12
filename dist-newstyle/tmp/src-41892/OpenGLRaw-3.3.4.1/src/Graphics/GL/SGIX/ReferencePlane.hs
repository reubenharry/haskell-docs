{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.ReferencePlane
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.ReferencePlane (
  -- * Extension Support
  glGetSGIXReferencePlane,
  gl_SGIX_reference_plane,
  -- * Enums
  pattern GL_REFERENCE_PLANE_EQUATION_SGIX,
  pattern GL_REFERENCE_PLANE_SGIX,
  -- * Functions
  glReferencePlaneSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
