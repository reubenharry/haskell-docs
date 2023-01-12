{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TimerQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TimerQuery (
  -- * Extension Support
  glGetEXTTimerQuery,
  gl_EXT_timer_query,
  -- * Enums
  pattern GL_TIME_ELAPSED_EXT,
  -- * Functions
  glGetQueryObjecti64vEXT,
  glGetQueryObjectui64vEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
