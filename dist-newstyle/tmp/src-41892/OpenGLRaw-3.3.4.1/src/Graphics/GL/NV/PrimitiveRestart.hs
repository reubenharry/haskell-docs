{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.PrimitiveRestart
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.PrimitiveRestart (
  -- * Extension Support
  glGetNVPrimitiveRestart,
  gl_NV_primitive_restart,
  -- * Enums
  pattern GL_PRIMITIVE_RESTART_INDEX_NV,
  pattern GL_PRIMITIVE_RESTART_NV,
  -- * Functions
  glPrimitiveRestartIndexNV,
  glPrimitiveRestartNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
