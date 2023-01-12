{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TransformFeedbackOverflowQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TransformFeedbackOverflowQuery (
  -- * Extension Support
  glGetARBTransformFeedbackOverflowQuery,
  gl_ARB_transform_feedback_overflow_query,
  -- * Enums
  pattern GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB,
  pattern GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
