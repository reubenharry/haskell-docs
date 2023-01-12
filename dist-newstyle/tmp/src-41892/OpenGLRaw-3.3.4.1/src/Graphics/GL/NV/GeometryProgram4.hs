{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.GeometryProgram4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.GeometryProgram4 (
  -- * Extension Support
  glGetNVGeometryProgram4,
  gl_NV_geometry_program4,
  -- * Enums
  pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT,
  pattern GL_GEOMETRY_INPUT_TYPE_EXT,
  pattern GL_GEOMETRY_OUTPUT_TYPE_EXT,
  pattern GL_GEOMETRY_PROGRAM_NV,
  pattern GL_GEOMETRY_VERTICES_OUT_EXT,
  pattern GL_LINES_ADJACENCY_EXT,
  pattern GL_LINE_STRIP_ADJACENCY_EXT,
  pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT,
  pattern GL_MAX_PROGRAM_OUTPUT_VERTICES_NV,
  pattern GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV,
  pattern GL_PROGRAM_POINT_SIZE_EXT,
  pattern GL_TRIANGLES_ADJACENCY_EXT,
  pattern GL_TRIANGLE_STRIP_ADJACENCY_EXT,
  -- * Functions
  glFramebufferTextureEXT,
  glFramebufferTextureFaceEXT,
  glFramebufferTextureLayerEXT,
  glProgramVertexLimitNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
