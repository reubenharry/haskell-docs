{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.PixelFormatFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.PixelFormatFloat (
  -- * Extension Support
  glGetATIPixelFormatFloat,
  gl_ATI_pixel_format_float,
  -- * Enums
  pattern GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI,
  pattern GL_RGBA_FLOAT_MODE_ATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
