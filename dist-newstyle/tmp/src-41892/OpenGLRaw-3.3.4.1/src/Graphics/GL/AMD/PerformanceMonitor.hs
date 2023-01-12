{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.PerformanceMonitor
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.PerformanceMonitor (
  -- * Extension Support
  glGetAMDPerformanceMonitor,
  gl_AMD_performance_monitor,
  -- * Enums
  pattern GL_COUNTER_RANGE_AMD,
  pattern GL_COUNTER_TYPE_AMD,
  pattern GL_PERCENTAGE_AMD,
  pattern GL_PERFMON_RESULT_AMD,
  pattern GL_PERFMON_RESULT_AVAILABLE_AMD,
  pattern GL_PERFMON_RESULT_SIZE_AMD,
  pattern GL_UNSIGNED_INT64_AMD,
  -- * Functions
  glBeginPerfMonitorAMD,
  glDeletePerfMonitorsAMD,
  glEndPerfMonitorAMD,
  glGenPerfMonitorsAMD,
  glGetPerfMonitorCounterDataAMD,
  glGetPerfMonitorCounterInfoAMD,
  glGetPerfMonitorCounterStringAMD,
  glGetPerfMonitorCountersAMD,
  glGetPerfMonitorGroupStringAMD,
  glGetPerfMonitorGroupsAMD,
  glSelectPerfMonitorCountersAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
