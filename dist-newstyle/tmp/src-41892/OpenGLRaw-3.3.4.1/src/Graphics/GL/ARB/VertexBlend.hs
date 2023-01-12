{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.VertexBlend
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.VertexBlend (
  -- * Extension Support
  glGetARBVertexBlend,
  gl_ARB_vertex_blend,
  -- * Enums
  pattern GL_ACTIVE_VERTEX_UNITS_ARB,
  pattern GL_CURRENT_WEIGHT_ARB,
  pattern GL_MAX_VERTEX_UNITS_ARB,
  pattern GL_MODELVIEW0_ARB,
  pattern GL_MODELVIEW10_ARB,
  pattern GL_MODELVIEW11_ARB,
  pattern GL_MODELVIEW12_ARB,
  pattern GL_MODELVIEW13_ARB,
  pattern GL_MODELVIEW14_ARB,
  pattern GL_MODELVIEW15_ARB,
  pattern GL_MODELVIEW16_ARB,
  pattern GL_MODELVIEW17_ARB,
  pattern GL_MODELVIEW18_ARB,
  pattern GL_MODELVIEW19_ARB,
  pattern GL_MODELVIEW1_ARB,
  pattern GL_MODELVIEW20_ARB,
  pattern GL_MODELVIEW21_ARB,
  pattern GL_MODELVIEW22_ARB,
  pattern GL_MODELVIEW23_ARB,
  pattern GL_MODELVIEW24_ARB,
  pattern GL_MODELVIEW25_ARB,
  pattern GL_MODELVIEW26_ARB,
  pattern GL_MODELVIEW27_ARB,
  pattern GL_MODELVIEW28_ARB,
  pattern GL_MODELVIEW29_ARB,
  pattern GL_MODELVIEW2_ARB,
  pattern GL_MODELVIEW30_ARB,
  pattern GL_MODELVIEW31_ARB,
  pattern GL_MODELVIEW3_ARB,
  pattern GL_MODELVIEW4_ARB,
  pattern GL_MODELVIEW5_ARB,
  pattern GL_MODELVIEW6_ARB,
  pattern GL_MODELVIEW7_ARB,
  pattern GL_MODELVIEW8_ARB,
  pattern GL_MODELVIEW9_ARB,
  pattern GL_VERTEX_BLEND_ARB,
  pattern GL_WEIGHT_ARRAY_ARB,
  pattern GL_WEIGHT_ARRAY_POINTER_ARB,
  pattern GL_WEIGHT_ARRAY_SIZE_ARB,
  pattern GL_WEIGHT_ARRAY_STRIDE_ARB,
  pattern GL_WEIGHT_ARRAY_TYPE_ARB,
  pattern GL_WEIGHT_SUM_UNITY_ARB,
  -- * Functions
  glVertexBlendARB,
  glWeightPointerARB,
  glWeightbvARB,
  glWeightdvARB,
  glWeightfvARB,
  glWeightivARB,
  glWeightsvARB,
  glWeightubvARB,
  glWeightuivARB,
  glWeightusvARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
