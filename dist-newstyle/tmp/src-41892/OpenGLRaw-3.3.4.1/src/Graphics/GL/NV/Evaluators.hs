{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.Evaluators
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.Evaluators (
  -- * Extension Support
  glGetNVEvaluators,
  gl_NV_evaluators,
  -- * Enums
  pattern GL_EVAL_2D_NV,
  pattern GL_EVAL_FRACTIONAL_TESSELLATION_NV,
  pattern GL_EVAL_TRIANGULAR_2D_NV,
  pattern GL_EVAL_VERTEX_ATTRIB0_NV,
  pattern GL_EVAL_VERTEX_ATTRIB10_NV,
  pattern GL_EVAL_VERTEX_ATTRIB11_NV,
  pattern GL_EVAL_VERTEX_ATTRIB12_NV,
  pattern GL_EVAL_VERTEX_ATTRIB13_NV,
  pattern GL_EVAL_VERTEX_ATTRIB14_NV,
  pattern GL_EVAL_VERTEX_ATTRIB15_NV,
  pattern GL_EVAL_VERTEX_ATTRIB1_NV,
  pattern GL_EVAL_VERTEX_ATTRIB2_NV,
  pattern GL_EVAL_VERTEX_ATTRIB3_NV,
  pattern GL_EVAL_VERTEX_ATTRIB4_NV,
  pattern GL_EVAL_VERTEX_ATTRIB5_NV,
  pattern GL_EVAL_VERTEX_ATTRIB6_NV,
  pattern GL_EVAL_VERTEX_ATTRIB7_NV,
  pattern GL_EVAL_VERTEX_ATTRIB8_NV,
  pattern GL_EVAL_VERTEX_ATTRIB9_NV,
  pattern GL_MAP_ATTRIB_U_ORDER_NV,
  pattern GL_MAP_ATTRIB_V_ORDER_NV,
  pattern GL_MAP_TESSELLATION_NV,
  pattern GL_MAX_MAP_TESSELLATION_NV,
  pattern GL_MAX_RATIONAL_EVAL_ORDER_NV,
  -- * Functions
  glEvalMapsNV,
  glGetMapAttribParameterfvNV,
  glGetMapAttribParameterivNV,
  glGetMapControlPointsNV,
  glGetMapParameterfvNV,
  glGetMapParameterivNV,
  glMapControlPointsNV,
  glMapParameterfvNV,
  glMapParameterivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
