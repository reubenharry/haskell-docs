{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.RobustnessCore
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.RobustnessCore (
  -- * Extension Support
  glGetARBRobustness,
  gl_ARB_robustness,
  -- * Enums
  pattern GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB,
  pattern GL_GUILTY_CONTEXT_RESET_ARB,
  pattern GL_INNOCENT_CONTEXT_RESET_ARB,
  pattern GL_LOSE_CONTEXT_ON_RESET_ARB,
  pattern GL_NO_ERROR,
  pattern GL_NO_RESET_NOTIFICATION_ARB,
  pattern GL_RESET_NOTIFICATION_STRATEGY_ARB,
  pattern GL_UNKNOWN_CONTEXT_RESET_ARB,
  -- * Functions
  glGetGraphicsResetStatusARB,
  glGetnCompressedTexImageARB,
  glGetnTexImageARB,
  glGetnUniformdvARB,
  glGetnUniformfvARB,
  glGetnUniformivARB,
  glGetnUniformuivARB,
  glReadnPixelsARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
