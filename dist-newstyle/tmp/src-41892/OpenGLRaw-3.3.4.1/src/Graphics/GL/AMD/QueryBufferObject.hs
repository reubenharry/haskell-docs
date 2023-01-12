{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.QueryBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.QueryBufferObject (
  -- * Extension Support
  glGetAMDQueryBufferObject,
  gl_AMD_query_buffer_object,
  -- * Enums
  pattern GL_QUERY_BUFFER_AMD,
  pattern GL_QUERY_BUFFER_BINDING_AMD,
  pattern GL_QUERY_RESULT_NO_WAIT_AMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
