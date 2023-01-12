{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.Robustness
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.Robustness (
  -- * Extension Support
  glGetKHRRobustness,
  gl_KHR_robustness,
  -- * Enums
  pattern GL_CONTEXT_LOST,
  pattern GL_CONTEXT_ROBUST_ACCESS,
  pattern GL_GUILTY_CONTEXT_RESET,
  pattern GL_INNOCENT_CONTEXT_RESET,
  pattern GL_LOSE_CONTEXT_ON_RESET,
  pattern GL_NO_ERROR,
  pattern GL_NO_RESET_NOTIFICATION,
  pattern GL_RESET_NOTIFICATION_STRATEGY,
  pattern GL_UNKNOWN_CONTEXT_RESET,
  -- * Functions
  glGetGraphicsResetStatus,
  glGetnUniformfv,
  glGetnUniformiv,
  glGetnUniformuiv,
  glReadnPixels
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
