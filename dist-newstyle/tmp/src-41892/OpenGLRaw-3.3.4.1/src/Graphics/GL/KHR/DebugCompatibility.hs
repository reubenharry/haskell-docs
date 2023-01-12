{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.KHR.DebugCompatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.KHR.DebugCompatibility (
  -- * Extension Support
  glGetKHRDebug,
  gl_KHR_debug,
  -- * Enums
  pattern GL_BUFFER,
  pattern GL_CONTEXT_FLAG_DEBUG_BIT,
  pattern GL_DEBUG_CALLBACK_FUNCTION,
  pattern GL_DEBUG_CALLBACK_USER_PARAM,
  pattern GL_DEBUG_GROUP_STACK_DEPTH,
  pattern GL_DEBUG_LOGGED_MESSAGES,
  pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH,
  pattern GL_DEBUG_OUTPUT,
  pattern GL_DEBUG_OUTPUT_SYNCHRONOUS,
  pattern GL_DEBUG_SEVERITY_HIGH,
  pattern GL_DEBUG_SEVERITY_LOW,
  pattern GL_DEBUG_SEVERITY_MEDIUM,
  pattern GL_DEBUG_SEVERITY_NOTIFICATION,
  pattern GL_DEBUG_SOURCE_API,
  pattern GL_DEBUG_SOURCE_APPLICATION,
  pattern GL_DEBUG_SOURCE_OTHER,
  pattern GL_DEBUG_SOURCE_SHADER_COMPILER,
  pattern GL_DEBUG_SOURCE_THIRD_PARTY,
  pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM,
  pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR,
  pattern GL_DEBUG_TYPE_ERROR,
  pattern GL_DEBUG_TYPE_MARKER,
  pattern GL_DEBUG_TYPE_OTHER,
  pattern GL_DEBUG_TYPE_PERFORMANCE,
  pattern GL_DEBUG_TYPE_POP_GROUP,
  pattern GL_DEBUG_TYPE_PORTABILITY,
  pattern GL_DEBUG_TYPE_PUSH_GROUP,
  pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR,
  pattern GL_DISPLAY_LIST,
  pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH,
  pattern GL_MAX_DEBUG_LOGGED_MESSAGES,
  pattern GL_MAX_DEBUG_MESSAGE_LENGTH,
  pattern GL_MAX_LABEL_LENGTH,
  pattern GL_PROGRAM,
  pattern GL_PROGRAM_PIPELINE,
  pattern GL_QUERY,
  pattern GL_SAMPLER,
  pattern GL_SHADER,
  pattern GL_STACK_OVERFLOW,
  pattern GL_STACK_UNDERFLOW,
  pattern GL_VERTEX_ARRAY,
  -- * Functions
  glDebugMessageCallback,
  glDebugMessageControl,
  glDebugMessageInsert,
  glGetDebugMessageLog,
  glGetObjectLabel,
  glGetObjectPtrLabel,
  glGetPointerv,
  glObjectLabel,
  glObjectPtrLabel,
  glPopDebugGroup,
  glPushDebugGroup
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
