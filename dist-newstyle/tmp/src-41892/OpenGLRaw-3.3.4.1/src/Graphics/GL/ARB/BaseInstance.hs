--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.BaseInstance
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.BaseInstance (
  -- * Extension Support
  glGetARBBaseInstance,
  gl_ARB_base_instance,
  -- * Functions
  glDrawArraysInstancedBaseInstance,
  glDrawElementsInstancedBaseInstance,
  glDrawElementsInstancedBaseVertexBaseInstance
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
