{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.CLEvent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.CLEvent (
  -- * Extension Support
  glGetARBCLEvent,
  gl_ARB_cl_event,
  -- * Enums
  pattern GL_SYNC_CL_EVENT_ARB,
  pattern GL_SYNC_CL_EVENT_COMPLETE_ARB,
  -- * Functions
  glCreateSyncFromCLeventARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
