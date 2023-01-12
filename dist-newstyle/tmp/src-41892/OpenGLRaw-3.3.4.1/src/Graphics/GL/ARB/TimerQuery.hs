{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TimerQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TimerQuery (
  -- * Extension Support
  glGetARBTimerQuery,
  gl_ARB_timer_query,
  -- * Enums
  pattern GL_TIMESTAMP,
  pattern GL_TIME_ELAPSED,
  -- * Functions
  glGetQueryObjecti64v,
  glGetQueryObjectui64v,
  glQueryCounter
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
