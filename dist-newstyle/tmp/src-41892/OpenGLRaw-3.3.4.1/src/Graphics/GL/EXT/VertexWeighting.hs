{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.VertexWeighting
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.VertexWeighting (
  -- * Extension Support
  glGetEXTVertexWeighting,
  gl_EXT_vertex_weighting,
  -- * Enums
  pattern GL_CURRENT_VERTEX_WEIGHT_EXT,
  pattern GL_MODELVIEW0_EXT,
  pattern GL_MODELVIEW0_MATRIX_EXT,
  pattern GL_MODELVIEW0_STACK_DEPTH_EXT,
  pattern GL_MODELVIEW1_EXT,
  pattern GL_MODELVIEW1_MATRIX_EXT,
  pattern GL_MODELVIEW1_STACK_DEPTH_EXT,
  pattern GL_VERTEX_WEIGHTING_EXT,
  pattern GL_VERTEX_WEIGHT_ARRAY_EXT,
  pattern GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT,
  pattern GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT,
  pattern GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT,
  pattern GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT,
  -- * Functions
  glVertexWeightPointerEXT,
  glVertexWeightfEXT,
  glVertexWeightfvEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
