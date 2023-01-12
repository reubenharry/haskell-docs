{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.DebugOutput
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.DebugOutput (
  -- * Extension Support
  glGetARBDebugOutput,
  gl_ARB_debug_output,
  -- * Enums
  pattern GL_DEBUG_CALLBACK_FUNCTION_ARB,
  pattern GL_DEBUG_CALLBACK_USER_PARAM_ARB,
  pattern GL_DEBUG_LOGGED_MESSAGES_ARB,
  pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB,
  pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB,
  pattern GL_DEBUG_SEVERITY_HIGH_ARB,
  pattern GL_DEBUG_SEVERITY_LOW_ARB,
  pattern GL_DEBUG_SEVERITY_MEDIUM_ARB,
  pattern GL_DEBUG_SOURCE_API_ARB,
  pattern GL_DEBUG_SOURCE_APPLICATION_ARB,
  pattern GL_DEBUG_SOURCE_OTHER_ARB,
  pattern GL_DEBUG_SOURCE_SHADER_COMPILER_ARB,
  pattern GL_DEBUG_SOURCE_THIRD_PARTY_ARB,
  pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB,
  pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB,
  pattern GL_DEBUG_TYPE_ERROR_ARB,
  pattern GL_DEBUG_TYPE_OTHER_ARB,
  pattern GL_DEBUG_TYPE_PERFORMANCE_ARB,
  pattern GL_DEBUG_TYPE_PORTABILITY_ARB,
  pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB,
  pattern GL_MAX_DEBUG_LOGGED_MESSAGES_ARB,
  pattern GL_MAX_DEBUG_MESSAGE_LENGTH_ARB,
  -- * Functions
  glDebugMessageCallbackARB,
  glDebugMessageControlARB,
  glDebugMessageInsertARB,
  glGetDebugMessageLogARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
