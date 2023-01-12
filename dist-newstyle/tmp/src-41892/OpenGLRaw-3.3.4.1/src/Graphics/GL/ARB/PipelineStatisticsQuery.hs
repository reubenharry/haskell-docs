{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.PipelineStatisticsQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.PipelineStatisticsQuery (
  -- * Extension Support
  glGetARBPipelineStatisticsQuery,
  gl_ARB_pipeline_statistics_query,
  -- * Enums
  pattern GL_CLIPPING_INPUT_PRIMITIVES_ARB,
  pattern GL_CLIPPING_OUTPUT_PRIMITIVES_ARB,
  pattern GL_COMPUTE_SHADER_INVOCATIONS_ARB,
  pattern GL_FRAGMENT_SHADER_INVOCATIONS_ARB,
  pattern GL_GEOMETRY_SHADER_INVOCATIONS,
  pattern GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB,
  pattern GL_PRIMITIVES_SUBMITTED_ARB,
  pattern GL_TESS_CONTROL_SHADER_PATCHES_ARB,
  pattern GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB,
  pattern GL_VERTEX_SHADER_INVOCATIONS_ARB,
  pattern GL_VERTICES_SUBMITTED_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
