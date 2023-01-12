{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PixelTransform
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PixelTransform (
  -- * Extension Support
  glGetEXTPixelTransform,
  gl_EXT_pixel_transform,
  -- * Enums
  pattern GL_AVERAGE_EXT,
  pattern GL_CUBIC_EXT,
  pattern GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT,
  pattern GL_PIXEL_CUBIC_WEIGHT_EXT,
  pattern GL_PIXEL_MAG_FILTER_EXT,
  pattern GL_PIXEL_MIN_FILTER_EXT,
  pattern GL_PIXEL_TRANSFORM_2D_EXT,
  pattern GL_PIXEL_TRANSFORM_2D_MATRIX_EXT,
  pattern GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT,
  -- * Functions
  glGetPixelTransformParameterfvEXT,
  glGetPixelTransformParameterivEXT,
  glPixelTransformParameterfEXT,
  glPixelTransformParameterfvEXT,
  glPixelTransformParameteriEXT,
  glPixelTransformParameterivEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
