{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.ShaderSubgroup
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.ShaderSubgroup (
  -- * Extension Support
  glGetKHRShaderSubgroup,
  gl_KHR_shader_subgroup,
  -- * Enums
  pattern GL_SUBGROUP_FEATURE_ARITHMETIC_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_BALLOT_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_BASIC_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_CLUSTERED_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_QUAD_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_SHUFFLE_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT_KHR,
  pattern GL_SUBGROUP_FEATURE_VOTE_BIT_KHR,
  pattern GL_SUBGROUP_QUAD_ALL_STAGES_KHR,
  pattern GL_SUBGROUP_SIZE_KHR,
  pattern GL_SUBGROUP_SUPPORTED_FEATURES_KHR,
  pattern GL_SUBGROUP_SUPPORTED_STAGES_KHR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
