{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexShader (
  -- * Extension Support
  glGetARBVertexShader,
  gl_ARB_vertex_shader,
  -- * Enums
  pattern GL_CURRENT_VERTEX_ATTRIB_ARB,
  pattern GL_FLOAT,
  pattern GL_FLOAT_MAT2_ARB,
  pattern GL_FLOAT_MAT3_ARB,
  pattern GL_FLOAT_MAT4_ARB,
  pattern GL_FLOAT_VEC2_ARB,
  pattern GL_FLOAT_VEC3_ARB,
  pattern GL_FLOAT_VEC4_ARB,
  pattern GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB,
  pattern GL_MAX_TEXTURE_COORDS_ARB,
  pattern GL_MAX_TEXTURE_IMAGE_UNITS_ARB,
  pattern GL_MAX_VARYING_FLOATS_ARB,
  pattern GL_MAX_VERTEX_ATTRIBS_ARB,
  pattern GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB,
  pattern GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB,
  pattern GL_OBJECT_ACTIVE_ATTRIBUTES_ARB,
  pattern GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB,
  pattern GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB,
  pattern GL_VERTEX_PROGRAM_POINT_SIZE_ARB,
  pattern GL_VERTEX_PROGRAM_TWO_SIDE_ARB,
  pattern GL_VERTEX_SHADER_ARB,
  -- * Functions
  glBindAttribLocationARB,
  glDisableVertexAttribArrayARB,
  glEnableVertexAttribArrayARB,
  glGetActiveAttribARB,
  glGetAttribLocationARB,
  glGetVertexAttribPointervARB,
  glGetVertexAttribdvARB,
  glGetVertexAttribfvARB,
  glGetVertexAttribivARB,
  glVertexAttrib1dARB,
  glVertexAttrib1dvARB,
  glVertexAttrib1fARB,
  glVertexAttrib1fvARB,
  glVertexAttrib1sARB,
  glVertexAttrib1svARB,
  glVertexAttrib2dARB,
  glVertexAttrib2dvARB,
  glVertexAttrib2fARB,
  glVertexAttrib2fvARB,
  glVertexAttrib2sARB,
  glVertexAttrib2svARB,
  glVertexAttrib3dARB,
  glVertexAttrib3dvARB,
  glVertexAttrib3fARB,
  glVertexAttrib3fvARB,
  glVertexAttrib3sARB,
  glVertexAttrib3svARB,
  glVertexAttrib4NbvARB,
  glVertexAttrib4NivARB,
  glVertexAttrib4NsvARB,
  glVertexAttrib4NubARB,
  glVertexAttrib4NubvARB,
  glVertexAttrib4NuivARB,
  glVertexAttrib4NusvARB,
  glVertexAttrib4bvARB,
  glVertexAttrib4dARB,
  glVertexAttrib4dvARB,
  glVertexAttrib4fARB,
  glVertexAttrib4fvARB,
  glVertexAttrib4ivARB,
  glVertexAttrib4sARB,
  glVertexAttrib4svARB,
  glVertexAttrib4ubvARB,
  glVertexAttrib4uivARB,
  glVertexAttrib4usvARB,
  glVertexAttribPointerARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
