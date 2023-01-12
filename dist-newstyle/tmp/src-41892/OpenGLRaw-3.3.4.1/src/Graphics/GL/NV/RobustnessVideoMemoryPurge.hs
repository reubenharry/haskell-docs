{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.RobustnessVideoMemoryPurge
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.RobustnessVideoMemoryPurge (
  -- * Extension Support
  glGetNVRobustnessVideoMemoryPurge,
  gl_NV_robustness_video_memory_purge,
  -- * Enums
  pattern GL_PURGED_CONTEXT_RESET_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
