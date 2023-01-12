--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TransformFeedbackInstanced
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TransformFeedbackInstanced (
  -- * Extension Support
  glGetARBTransformFeedbackInstanced,
  gl_ARB_transform_feedback_instanced,
  -- * Functions
  glDrawTransformFeedbackInstanced,
  glDrawTransformFeedbackStreamInstanced
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
