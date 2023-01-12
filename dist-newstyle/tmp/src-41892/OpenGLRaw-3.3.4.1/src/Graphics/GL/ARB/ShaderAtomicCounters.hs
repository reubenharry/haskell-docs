{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ShaderAtomicCounters
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ShaderAtomicCounters (
  -- * Extension Support
  glGetARBShaderAtomicCounters,
  gl_ARB_shader_atomic_counters,
  -- * Enums
  pattern GL_ACTIVE_ATOMIC_COUNTER_BUFFERS,
  pattern GL_ATOMIC_COUNTER_BUFFER,
  pattern GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS,
  pattern GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES,
  pattern GL_ATOMIC_COUNTER_BUFFER_BINDING,
  pattern GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER,
  pattern GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER,
  pattern GL_ATOMIC_COUNTER_BUFFER_SIZE,
  pattern GL_ATOMIC_COUNTER_BUFFER_START,
  pattern GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS,
  pattern GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE,
  pattern GL_MAX_COMBINED_ATOMIC_COUNTERS,
  pattern GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_FRAGMENT_ATOMIC_COUNTERS,
  pattern GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_GEOMETRY_ATOMIC_COUNTERS,
  pattern GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS,
  pattern GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS,
  pattern GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS,
  pattern GL_MAX_VERTEX_ATOMIC_COUNTERS,
  pattern GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS,
  pattern GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX,
  pattern GL_UNSIGNED_INT_ATOMIC_COUNTER,
  -- * Functions
  glGetActiveAtomicCounterBufferiv
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
