{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ScissorExclusive
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ScissorExclusive (
  -- * Extension Support
  glGetNVScissorExclusive,
  gl_NV_scissor_exclusive,
  -- * Enums
  pattern GL_SCISSOR_BOX_EXCLUSIVE_NV,
  pattern GL_SCISSOR_TEST_EXCLUSIVE_NV,
  -- * Functions
  glScissorExclusiveArrayvNV,
  glScissorExclusiveNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
