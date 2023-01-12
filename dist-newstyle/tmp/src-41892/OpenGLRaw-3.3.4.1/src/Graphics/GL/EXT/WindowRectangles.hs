{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.WindowRectangles
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.WindowRectangles (
  -- * Extension Support
  glGetEXTWindowRectangles,
  gl_EXT_window_rectangles,
  -- * Enums
  pattern GL_EXCLUSIVE_EXT,
  pattern GL_INCLUSIVE_EXT,
  pattern GL_MAX_WINDOW_RECTANGLES_EXT,
  pattern GL_NUM_WINDOW_RECTANGLES_EXT,
  pattern GL_WINDOW_RECTANGLE_EXT,
  pattern GL_WINDOW_RECTANGLE_MODE_EXT,
  -- * Functions
  glWindowRectanglesEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
