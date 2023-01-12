{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.DebugOutput
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.DebugOutput (
  -- * Extension Support
  glGetAMDDebugOutput,
  gl_AMD_debug_output,
  -- * Enums
  pattern GL_DEBUG_CATEGORY_API_ERROR_AMD,
  pattern GL_DEBUG_CATEGORY_APPLICATION_AMD,
  pattern GL_DEBUG_CATEGORY_DEPRECATION_AMD,
  pattern GL_DEBUG_CATEGORY_OTHER_AMD,
  pattern GL_DEBUG_CATEGORY_PERFORMANCE_AMD,
  pattern GL_DEBUG_CATEGORY_SHADER_COMPILER_AMD,
  pattern GL_DEBUG_CATEGORY_UNDEFINED_BEHAVIOR_AMD,
  pattern GL_DEBUG_CATEGORY_WINDOW_SYSTEM_AMD,
  pattern GL_DEBUG_LOGGED_MESSAGES_AMD,
  pattern GL_DEBUG_SEVERITY_HIGH_AMD,
  pattern GL_DEBUG_SEVERITY_LOW_AMD,
  pattern GL_DEBUG_SEVERITY_MEDIUM_AMD,
  pattern GL_MAX_DEBUG_LOGGED_MESSAGES_AMD,
  pattern GL_MAX_DEBUG_MESSAGE_LENGTH_AMD,
  -- * Functions
  glDebugMessageCallbackAMD,
  glDebugMessageEnableAMD,
  glDebugMessageInsertAMD,
  glGetDebugMessageLogAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
