{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TessellationShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TessellationShader (
  -- * Extension Support
  glGetARBTessellationShader,
  gl_ARB_tessellation_shader,
  -- * Enums
  pattern GL_CCW,
  pattern GL_CW,
  pattern GL_EQUAL,
  pattern GL_FRACTIONAL_EVEN,
  pattern GL_FRACTIONAL_ODD,
  pattern GL_ISOLINES,
  pattern GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS,
  pattern GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS,
  pattern GL_MAX_PATCH_VERTICES,
  pattern GL_MAX_TESS_CONTROL_INPUT_COMPONENTS,
  pattern GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS,
  pattern GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS,
  pattern GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS,
  pattern GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS,
  pattern GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS,
  pattern GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS,
  pattern GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS,
  pattern GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS,
  pattern GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS,
  pattern GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS,
  pattern GL_MAX_TESS_GEN_LEVEL,
  pattern GL_MAX_TESS_PATCH_COMPONENTS,
  pattern GL_PATCHES,
  pattern GL_PATCH_DEFAULT_INNER_LEVEL,
  pattern GL_PATCH_DEFAULT_OUTER_LEVEL,
  pattern GL_PATCH_VERTICES,
  pattern GL_QUADS,
  pattern GL_TESS_CONTROL_OUTPUT_VERTICES,
  pattern GL_TESS_CONTROL_SHADER,
  pattern GL_TESS_EVALUATION_SHADER,
  pattern GL_TESS_GEN_MODE,
  pattern GL_TESS_GEN_POINT_MODE,
  pattern GL_TESS_GEN_SPACING,
  pattern GL_TESS_GEN_VERTEX_ORDER,
  pattern GL_TRIANGLES,
  pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER,
  pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER,
  -- * Functions
  glPatchParameterfv,
  glPatchParameteri
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
