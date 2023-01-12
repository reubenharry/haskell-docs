{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.SecondaryColor
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.SecondaryColor (
  -- * Extension Support
  glGetEXTSecondaryColor,
  gl_EXT_secondary_color,
  -- * Enums
  pattern GL_COLOR_SUM_EXT,
  pattern GL_CURRENT_SECONDARY_COLOR_EXT,
  pattern GL_SECONDARY_COLOR_ARRAY_EXT,
  pattern GL_SECONDARY_COLOR_ARRAY_POINTER_EXT,
  pattern GL_SECONDARY_COLOR_ARRAY_SIZE_EXT,
  pattern GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT,
  pattern GL_SECONDARY_COLOR_ARRAY_TYPE_EXT,
  -- * Functions
  glSecondaryColor3bEXT,
  glSecondaryColor3bvEXT,
  glSecondaryColor3dEXT,
  glSecondaryColor3dvEXT,
  glSecondaryColor3fEXT,
  glSecondaryColor3fvEXT,
  glSecondaryColor3iEXT,
  glSecondaryColor3ivEXT,
  glSecondaryColor3sEXT,
  glSecondaryColor3svEXT,
  glSecondaryColor3ubEXT,
  glSecondaryColor3ubvEXT,
  glSecondaryColor3uiEXT,
  glSecondaryColor3uivEXT,
  glSecondaryColor3usEXT,
  glSecondaryColor3usvEXT,
  glSecondaryColorPointerEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
