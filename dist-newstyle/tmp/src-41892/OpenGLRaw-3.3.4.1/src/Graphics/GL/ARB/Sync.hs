{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.Sync
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.Sync (
  -- * Extension Support
  glGetARBSync,
  gl_ARB_sync,
  -- * Enums
  pattern GL_ALREADY_SIGNALED,
  pattern GL_CONDITION_SATISFIED,
  pattern GL_MAX_SERVER_WAIT_TIMEOUT,
  pattern GL_OBJECT_TYPE,
  pattern GL_SIGNALED,
  pattern GL_SYNC_CONDITION,
  pattern GL_SYNC_FENCE,
  pattern GL_SYNC_FLAGS,
  pattern GL_SYNC_FLUSH_COMMANDS_BIT,
  pattern GL_SYNC_GPU_COMMANDS_COMPLETE,
  pattern GL_SYNC_STATUS,
  pattern GL_TIMEOUT_EXPIRED,
  pattern GL_TIMEOUT_IGNORED,
  pattern GL_UNSIGNALED,
  pattern GL_WAIT_FAILED,
  -- * Functions
  glClientWaitSync,
  glDeleteSync,
  glFenceSync,
  glGetInteger64v,
  glGetSynciv,
  glIsSync,
  glWaitSync
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
