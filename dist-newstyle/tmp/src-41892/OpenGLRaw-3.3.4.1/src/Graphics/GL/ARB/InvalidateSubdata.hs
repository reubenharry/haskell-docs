--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.InvalidateSubdata
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.InvalidateSubdata (
  -- * Extension Support
  glGetARBInvalidateSubdata,
  gl_ARB_invalidate_subdata,
  -- * Functions
  glInvalidateBufferData,
  glInvalidateBufferSubData,
  glInvalidateFramebuffer,
  glInvalidateSubFramebuffer,
  glInvalidateTexImage,
  glInvalidateTexSubImage
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
