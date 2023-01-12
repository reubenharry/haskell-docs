{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GeometryShader4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GeometryShader4 (
  -- * Extension Support
  glGetARBGeometryShader4,
  gl_ARB_geometry_shader4,
  -- * Enums
  pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB,
  pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB,
  pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB,
  pattern GL_GEOMETRY_INPUT_TYPE_ARB,
  pattern GL_GEOMETRY_OUTPUT_TYPE_ARB,
  pattern GL_GEOMETRY_SHADER_ARB,
  pattern GL_GEOMETRY_VERTICES_OUT_ARB,
  pattern GL_LINES_ADJACENCY_ARB,
  pattern GL_LINE_STRIP_ADJACENCY_ARB,
  pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB,
  pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB,
  pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB,
  pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB,
  pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB,
  pattern GL_MAX_VARYING_COMPONENTS,
  pattern GL_MAX_VERTEX_VARYING_COMPONENTS_ARB,
  pattern GL_PROGRAM_POINT_SIZE_ARB,
  pattern GL_TRIANGLES_ADJACENCY_ARB,
  pattern GL_TRIANGLE_STRIP_ADJACENCY_ARB,
  -- * Functions
  glFramebufferTextureARB,
  glFramebufferTextureFaceARB,
  glFramebufferTextureLayerARB,
  glProgramParameteriARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
