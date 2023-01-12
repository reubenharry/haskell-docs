{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TransformFeedback
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TransformFeedback (
  -- * Extension Support
  glGetNVTransformFeedback,
  gl_NV_transform_feedback,
  -- * Enums
  pattern GL_ACTIVE_VARYINGS_NV,
  pattern GL_ACTIVE_VARYING_MAX_LENGTH_NV,
  pattern GL_BACK_PRIMARY_COLOR_NV,
  pattern GL_BACK_SECONDARY_COLOR_NV,
  pattern GL_CLIP_DISTANCE_NV,
  pattern GL_GENERIC_ATTRIB_NV,
  pattern GL_INTERLEAVED_ATTRIBS_NV,
  pattern GL_LAYER_NV,
  pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV,
  pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV,
  pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV,
  pattern GL_NEXT_BUFFER_NV,
  pattern GL_PRIMITIVES_GENERATED_NV,
  pattern GL_PRIMITIVE_ID_NV,
  pattern GL_RASTERIZER_DISCARD_NV,
  pattern GL_SEPARATE_ATTRIBS_NV,
  pattern GL_SKIP_COMPONENTS1_NV,
  pattern GL_SKIP_COMPONENTS2_NV,
  pattern GL_SKIP_COMPONENTS3_NV,
  pattern GL_SKIP_COMPONENTS4_NV,
  pattern GL_TEXTURE_COORD_NV,
  pattern GL_TRANSFORM_FEEDBACK_ATTRIBS_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_NV,
  pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV,
  pattern GL_TRANSFORM_FEEDBACK_RECORD_NV,
  pattern GL_TRANSFORM_FEEDBACK_VARYINGS_NV,
  pattern GL_VERTEX_ID_NV,
  -- * Functions
  glActiveVaryingNV,
  glBeginTransformFeedbackNV,
  glBindBufferBaseNV,
  glBindBufferOffsetNV,
  glBindBufferRangeNV,
  glEndTransformFeedbackNV,
  glGetActiveVaryingNV,
  glGetTransformFeedbackVaryingNV,
  glGetVaryingLocationNV,
  glTransformFeedbackAttribsNV,
  glTransformFeedbackStreamAttribsNV,
  glTransformFeedbackVaryingsNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
