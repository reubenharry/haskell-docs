{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.CoordinateFrame
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.CoordinateFrame (
  -- * Extension Support
  glGetEXTCoordinateFrame,
  gl_EXT_coordinate_frame,
  -- * Enums
  pattern GL_BINORMAL_ARRAY_EXT,
  pattern GL_BINORMAL_ARRAY_POINTER_EXT,
  pattern GL_BINORMAL_ARRAY_STRIDE_EXT,
  pattern GL_BINORMAL_ARRAY_TYPE_EXT,
  pattern GL_CURRENT_BINORMAL_EXT,
  pattern GL_CURRENT_TANGENT_EXT,
  pattern GL_MAP1_BINORMAL_EXT,
  pattern GL_MAP1_TANGENT_EXT,
  pattern GL_MAP2_BINORMAL_EXT,
  pattern GL_MAP2_TANGENT_EXT,
  pattern GL_TANGENT_ARRAY_EXT,
  pattern GL_TANGENT_ARRAY_POINTER_EXT,
  pattern GL_TANGENT_ARRAY_STRIDE_EXT,
  pattern GL_TANGENT_ARRAY_TYPE_EXT,
  -- * Functions
  glBinormal3bEXT,
  glBinormal3bvEXT,
  glBinormal3dEXT,
  glBinormal3dvEXT,
  glBinormal3fEXT,
  glBinormal3fvEXT,
  glBinormal3iEXT,
  glBinormal3ivEXT,
  glBinormal3sEXT,
  glBinormal3svEXT,
  glBinormalPointerEXT,
  glTangent3bEXT,
  glTangent3bvEXT,
  glTangent3dEXT,
  glTangent3dvEXT,
  glTangent3fEXT,
  glTangent3fvEXT,
  glTangent3iEXT,
  glTangent3ivEXT,
  glTangent3sEXT,
  glTangent3svEXT,
  glTangentPointerEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
