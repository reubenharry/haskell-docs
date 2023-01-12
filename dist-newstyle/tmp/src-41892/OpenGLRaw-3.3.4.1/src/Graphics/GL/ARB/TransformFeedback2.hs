{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TransformFeedback2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TransformFeedback2 (
  -- * Extension Support
  glGetARBTransformFeedback2,
  gl_ARB_transform_feedback2,
  -- * Enums
  pattern GL_TRANSFORM_FEEDBACK,
  pattern GL_TRANSFORM_FEEDBACK_BINDING,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED,
  -- * Functions
  glBindTransformFeedback,
  glDeleteTransformFeedbacks,
  glDrawTransformFeedback,
  glGenTransformFeedbacks,
  glIsTransformFeedback,
  glPauseTransformFeedback,
  glResumeTransformFeedback
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
