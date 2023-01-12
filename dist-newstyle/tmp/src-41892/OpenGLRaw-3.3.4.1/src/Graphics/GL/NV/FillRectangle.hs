{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.FillRectangle
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.FillRectangle (
  -- * Extension Support
  glGetNVFillRectangle,
  gl_NV_fill_rectangle,
  -- * Enums
  pattern GL_FILL_RECTANGLE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
