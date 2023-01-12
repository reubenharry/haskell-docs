{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SUN.TriangleList
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SUN.TriangleList (
  -- * Extension Support
  glGetSUNTriangleList,
  gl_SUN_triangle_list,
  -- * Enums
  pattern GL_R1UI_C3F_V3F_SUN,
  pattern GL_R1UI_C4F_N3F_V3F_SUN,
  pattern GL_R1UI_C4UB_V3F_SUN,
  pattern GL_R1UI_N3F_V3F_SUN,
  pattern GL_R1UI_T2F_C4F_N3F_V3F_SUN,
  pattern GL_R1UI_T2F_N3F_V3F_SUN,
  pattern GL_R1UI_T2F_V3F_SUN,
  pattern GL_R1UI_V3F_SUN,
  pattern GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN,
  pattern GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN,
  pattern GL_REPLACEMENT_CODE_ARRAY_SUN,
  pattern GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN,
  pattern GL_REPLACEMENT_CODE_SUN,
  pattern GL_REPLACE_MIDDLE_SUN,
  pattern GL_REPLACE_OLDEST_SUN,
  pattern GL_RESTART_SUN,
  pattern GL_TRIANGLE_LIST_SUN,
  -- * Functions
  glReplacementCodePointerSUN,
  glReplacementCodeubSUN,
  glReplacementCodeubvSUN,
  glReplacementCodeuiSUN,
  glReplacementCodeuivSUN,
  glReplacementCodeusSUN,
  glReplacementCodeusvSUN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
