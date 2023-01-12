{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VertexProgram3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VertexProgram3 (
  -- * Extension Support
  glGetNVVertexProgram3,
  gl_NV_vertex_program3,
  -- * Enums
  pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
