{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.RegisterCombiners2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.RegisterCombiners2 (
  -- * Extension Support
  glGetNVRegisterCombiners2,
  gl_NV_register_combiners2,
  -- * Enums
  pattern GL_PER_STAGE_CONSTANTS_NV,
  -- * Functions
  glCombinerStageParameterfvNV,
  glGetCombinerStageParameterfvNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
