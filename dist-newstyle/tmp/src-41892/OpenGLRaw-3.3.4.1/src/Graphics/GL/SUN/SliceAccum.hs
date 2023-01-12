{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.SliceAccum
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.SliceAccum (
  -- * Extension Support
  glGetSUNSliceAccum,
  gl_SUN_slice_accum,
  -- * Enums
  pattern GL_SLICE_ACCUM_SUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
