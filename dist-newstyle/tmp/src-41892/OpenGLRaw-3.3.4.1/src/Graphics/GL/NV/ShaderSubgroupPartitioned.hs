{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ShaderSubgroupPartitioned
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ShaderSubgroupPartitioned (
  -- * Extension Support
  glGetNVShaderSubgroupPartitioned,
  gl_NV_shader_subgroup_partitioned,
  -- * Enums
  pattern GL_SUBGROUP_FEATURE_PARTITIONED_BIT_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
