{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TransformFeedback
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TransformFeedback (
  -- * Extension Support
  glGetEXTTransformFeedback,
  gl_EXT_transform_feedback,
  -- * Enums
  pattern GL_INTERLEAVED_ATTRIBS_EXT,
  pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT,
  pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT,
  pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT,
  pattern GL_PRIMITIVES_GENERATED_EXT,
  pattern GL_RASTERIZER_DISCARD_EXT,
  pattern GL_SEPARATE_ATTRIBS_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT,
  pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT,
  pattern GL_TRANSFORM_FEEDBACK_VARYINGS_EXT,
  pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT,
  -- * Functions
  glBeginTransformFeedbackEXT,
  glBindBufferBaseEXT,
  glBindBufferOffsetEXT,
  glBindBufferRangeEXT,
  glEndTransformFeedbackEXT,
  glGetTransformFeedbackVaryingEXT,
  glTransformFeedbackVaryingsEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
