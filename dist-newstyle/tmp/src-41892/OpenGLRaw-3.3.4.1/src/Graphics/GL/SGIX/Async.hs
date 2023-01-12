{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Async
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Async (
  -- * Extension Support
  glGetSGIXAsync,
  gl_SGIX_async,
  -- * Enums
  pattern GL_ASYNC_MARKER_SGIX,
  -- * Functions
  glAsyncMarkerSGIX,
  glDeleteAsyncMarkersSGIX,
  glFinishAsyncSGIX,
  glGenAsyncMarkersSGIX,
  glIsAsyncMarkerSGIX,
  glPollAsyncSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
