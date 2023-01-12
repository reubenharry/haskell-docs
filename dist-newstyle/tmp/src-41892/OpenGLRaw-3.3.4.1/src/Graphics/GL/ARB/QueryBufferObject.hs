{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.QueryBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.QueryBufferObject (
  -- * Extension Support
  glGetARBQueryBufferObject,
  gl_ARB_query_buffer_object,
  -- * Enums
  pattern GL_QUERY_BUFFER,
  pattern GL_QUERY_BUFFER_BARRIER_BIT,
  pattern GL_QUERY_BUFFER_BINDING,
  pattern GL_QUERY_RESULT_NO_WAIT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
