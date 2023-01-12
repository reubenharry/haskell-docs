{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.HP.ImageTransform
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.HP.ImageTransform (
  -- * Extension Support
  glGetHPImageTransform,
  gl_HP_image_transform,
  -- * Enums
  pattern GL_AVERAGE_HP,
  pattern GL_CUBIC_HP,
  pattern GL_IMAGE_CUBIC_WEIGHT_HP,
  pattern GL_IMAGE_MAG_FILTER_HP,
  pattern GL_IMAGE_MIN_FILTER_HP,
  pattern GL_IMAGE_ROTATE_ANGLE_HP,
  pattern GL_IMAGE_ROTATE_ORIGIN_X_HP,
  pattern GL_IMAGE_ROTATE_ORIGIN_Y_HP,
  pattern GL_IMAGE_SCALE_X_HP,
  pattern GL_IMAGE_SCALE_Y_HP,
  pattern GL_IMAGE_TRANSFORM_2D_HP,
  pattern GL_IMAGE_TRANSLATE_X_HP,
  pattern GL_IMAGE_TRANSLATE_Y_HP,
  pattern GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP,
  pattern GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP,
  -- * Functions
  glGetImageTransformParameterfvHP,
  glGetImageTransformParameterivHP,
  glImageTransformParameterfHP,
  glImageTransformParameterfvHP,
  glImageTransformParameteriHP,
  glImageTransformParameterivHP
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
