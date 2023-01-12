{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TransformFeedback2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TransformFeedback2 (
  -- * Extension Support
  glGetNVTransformFeedback2,
  gl_NV_transform_feedback2,
  -- * Enums
  pattern GL_TRANSFORM_FEEDBACK_BINDING_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV,
  pattern GL_TRANSFORM_FEEDBACK_NV,
  -- * Functions
  glBindTransformFeedbackNV,
  glDeleteTransformFeedbacksNV,
  glDrawTransformFeedbackNV,
  glGenTransformFeedbacksNV,
  glIsTransformFeedbackNV,
  glPauseTransformFeedbackNV,
  glResumeTransformFeedbackNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
