{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DepthBoundsTest
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DepthBoundsTest (
  -- * Extension Support
  glGetEXTDepthBoundsTest,
  gl_EXT_depth_bounds_test,
  -- * Enums
  pattern GL_DEPTH_BOUNDS_EXT,
  pattern GL_DEPTH_BOUNDS_TEST_EXT,
  -- * Functions
  glDepthBoundsEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
