{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TransformFeedback3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TransformFeedback3 (
  -- * Extension Support
  glGetARBTransformFeedback3,
  gl_ARB_transform_feedback3,
  -- * Enums
  pattern GL_MAX_TRANSFORM_FEEDBACK_BUFFERS,
  pattern GL_MAX_VERTEX_STREAMS,
  -- * Functions
  glBeginQueryIndexed,
  glDrawTransformFeedbackStream,
  glEndQueryIndexed,
  glGetQueryIndexediv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
