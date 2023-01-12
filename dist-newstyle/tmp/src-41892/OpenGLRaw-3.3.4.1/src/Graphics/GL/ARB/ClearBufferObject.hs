--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ClearBufferObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ClearBufferObject (
  -- * Extension Support
  glGetARBClearBufferObject,
  gl_ARB_clear_buffer_object,
  -- * Functions
  glClearBufferData,
  glClearBufferSubData
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
