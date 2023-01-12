{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.MeshShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.MeshShader (
  -- * Extension Support
  glGetNVMeshShader,
  gl_NV_mesh_shader,
  -- * Enums
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_MESH_SHADER_NV,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TASK_SHADER_NV,
  pattern GL_MAX_COMBINED_MESH_UNIFORM_COMPONENTS_NV,
  pattern GL_MAX_COMBINED_TASK_UNIFORM_COMPONENTS_NV,
  pattern GL_MAX_DRAW_MESH_TASKS_COUNT_NV,
  pattern GL_MAX_MESH_ATOMIC_COUNTERS_NV,
  pattern GL_MAX_MESH_ATOMIC_COUNTER_BUFFERS_NV,
  pattern GL_MAX_MESH_IMAGE_UNIFORMS_NV,
  pattern GL_MAX_MESH_OUTPUT_PRIMITIVES_NV,
  pattern GL_MAX_MESH_OUTPUT_VERTICES_NV,
  pattern GL_MAX_MESH_SHADER_STORAGE_BLOCKS_NV,
  pattern GL_MAX_MESH_TEXTURE_IMAGE_UNITS_NV,
  pattern GL_MAX_MESH_TOTAL_MEMORY_SIZE_NV,
  pattern GL_MAX_MESH_UNIFORM_BLOCKS_NV,
  pattern GL_MAX_MESH_UNIFORM_COMPONENTS_NV,
  pattern GL_MAX_MESH_VIEWS_NV,
  pattern GL_MAX_MESH_WORK_GROUP_INVOCATIONS_NV,
  pattern GL_MAX_MESH_WORK_GROUP_SIZE_NV,
  pattern GL_MAX_TASK_ATOMIC_COUNTERS_NV,
  pattern GL_MAX_TASK_ATOMIC_COUNTER_BUFFERS_NV,
  pattern GL_MAX_TASK_IMAGE_UNIFORMS_NV,
  pattern GL_MAX_TASK_OUTPUT_COUNT_NV,
  pattern GL_MAX_TASK_SHADER_STORAGE_BLOCKS_NV,
  pattern GL_MAX_TASK_TEXTURE_IMAGE_UNITS_NV,
  pattern GL_MAX_TASK_TOTAL_MEMORY_SIZE_NV,
  pattern GL_MAX_TASK_UNIFORM_BLOCKS_NV,
  pattern GL_MAX_TASK_UNIFORM_COMPONENTS_NV,
  pattern GL_MAX_TASK_WORK_GROUP_INVOCATIONS_NV,
  pattern GL_MAX_TASK_WORK_GROUP_SIZE_NV,
  pattern GL_MESH_OUTPUT_PER_PRIMITIVE_GRANULARITY_NV,
  pattern GL_MESH_OUTPUT_PER_VERTEX_GRANULARITY_NV,
  pattern GL_MESH_OUTPUT_TYPE_NV,
  pattern GL_MESH_PRIMITIVES_OUT_NV,
  pattern GL_MESH_SHADER_BIT_NV,
  pattern GL_MESH_SHADER_NV,
  pattern GL_MESH_SUBROUTINE_NV,
  pattern GL_MESH_SUBROUTINE_UNIFORM_NV,
  pattern GL_MESH_VERTICES_OUT_NV,
  pattern GL_MESH_WORK_GROUP_SIZE_NV,
  pattern GL_REFERENCED_BY_MESH_SHADER_NV,
  pattern GL_REFERENCED_BY_TASK_SHADER_NV,
  pattern GL_TASK_SHADER_BIT_NV,
  pattern GL_TASK_SHADER_NV,
  pattern GL_TASK_SUBROUTINE_NV,
  pattern GL_TASK_SUBROUTINE_UNIFORM_NV,
  pattern GL_TASK_WORK_GROUP_SIZE_NV,
  pattern GL_UNIFORM_BLOCK_REFERENCED_BY_MESH_SHADER_NV,
  pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TASK_SHADER_NV,
  -- * Functions
  glDrawMeshTasksIndirectNV,
  glDrawMeshTasksNV,
  glMultiDrawMeshTasksIndirectCountNV,
  glMultiDrawMeshTasksIndirectNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
