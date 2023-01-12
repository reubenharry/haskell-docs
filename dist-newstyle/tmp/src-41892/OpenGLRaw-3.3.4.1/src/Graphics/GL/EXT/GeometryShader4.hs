{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.GeometryShader4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.GeometryShader4 (
  -- * Extension Support
  glGetEXTGeometryShader4,
  gl_EXT_geometry_shader4,
  -- * Enums
  pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT,
  pattern GL_GEOMETRY_INPUT_TYPE_EXT,
  pattern GL_GEOMETRY_OUTPUT_TYPE_EXT,
  pattern GL_GEOMETRY_SHADER_EXT,
  pattern GL_GEOMETRY_VERTICES_OUT_EXT,
  pattern GL_LINES_ADJACENCY_EXT,
  pattern GL_LINE_STRIP_ADJACENCY_EXT,
  pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT,
  pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT,
  pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT,
  pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT,
  pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT,
  pattern GL_MAX_VARYING_COMPONENTS_EXT,
  pattern GL_MAX_VERTEX_VARYING_COMPONENTS_EXT,
  pattern GL_PROGRAM_POINT_SIZE_EXT,
  pattern GL_TRIANGLES_ADJACENCY_EXT,
  pattern GL_TRIANGLE_STRIP_ADJACENCY_EXT,
  -- * Functions
  glProgramParameteriEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
