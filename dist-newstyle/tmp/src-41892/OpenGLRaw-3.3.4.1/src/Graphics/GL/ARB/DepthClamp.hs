{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DepthClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DepthClamp (
  -- * Extension Support
  glGetARBDepthClamp,
  gl_ARB_depth_clamp,
  -- * Enums
  pattern GL_DEPTH_CLAMP
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
