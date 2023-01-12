{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.PerformanceQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.PerformanceQuery (
  -- * Extension Support
  glGetINTELPerformanceQuery,
  gl_INTEL_performance_query,
  -- * Enums
  pattern GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL,
  pattern GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL,
  pattern GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL,
  pattern GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL,
  pattern GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL,
  pattern GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL,
  pattern GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL,
  pattern GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL,
  pattern GL_PERFQUERY_COUNTER_EVENT_INTEL,
  pattern GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL,
  pattern GL_PERFQUERY_COUNTER_RAW_INTEL,
  pattern GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL,
  pattern GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL,
  pattern GL_PERFQUERY_DONOT_FLUSH_INTEL,
  pattern GL_PERFQUERY_FLUSH_INTEL,
  pattern GL_PERFQUERY_GLOBAL_CONTEXT_INTEL,
  pattern GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL,
  pattern GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL,
  pattern GL_PERFQUERY_SINGLE_CONTEXT_INTEL,
  pattern GL_PERFQUERY_WAIT_INTEL,
  -- * Functions
  glBeginPerfQueryINTEL,
  glCreatePerfQueryINTEL,
  glDeletePerfQueryINTEL,
  glEndPerfQueryINTEL,
  glGetFirstPerfQueryIdINTEL,
  glGetNextPerfQueryIdINTEL,
  glGetPerfCounterInfoINTEL,
  glGetPerfQueryDataINTEL,
  glGetPerfQueryIdByNameINTEL,
  glGetPerfQueryInfoINTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
