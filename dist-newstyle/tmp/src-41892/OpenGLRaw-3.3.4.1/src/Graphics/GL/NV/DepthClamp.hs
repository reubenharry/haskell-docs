{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.DepthClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.DepthClamp (
  -- * Extension Support
  glGetNVDepthClamp,
  gl_NV_depth_clamp,
  -- * Enums
  pattern GL_DEPTH_CLAMP_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
