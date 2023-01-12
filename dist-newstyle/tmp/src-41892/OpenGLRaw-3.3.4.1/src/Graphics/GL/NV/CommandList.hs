{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.CommandList
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.CommandList (
  -- * Extension Support
  glGetNVCommandList,
  gl_NV_command_list,
  -- * Enums
  pattern GL_ALPHA_REF_COMMAND_NV,
  pattern GL_ATTRIBUTE_ADDRESS_COMMAND_NV,
  pattern GL_BLEND_COLOR_COMMAND_NV,
  pattern GL_DRAW_ARRAYS_COMMAND_NV,
  pattern GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV,
  pattern GL_DRAW_ARRAYS_STRIP_COMMAND_NV,
  pattern GL_DRAW_ELEMENTS_COMMAND_NV,
  pattern GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV,
  pattern GL_DRAW_ELEMENTS_STRIP_COMMAND_NV,
  pattern GL_ELEMENT_ADDRESS_COMMAND_NV,
  pattern GL_FRONT_FACE_COMMAND_NV,
  pattern GL_LINE_WIDTH_COMMAND_NV,
  pattern GL_NOP_COMMAND_NV,
  pattern GL_POLYGON_OFFSET_COMMAND_NV,
  pattern GL_SCISSOR_COMMAND_NV,
  pattern GL_STENCIL_REF_COMMAND_NV,
  pattern GL_TERMINATE_SEQUENCE_COMMAND_NV,
  pattern GL_UNIFORM_ADDRESS_COMMAND_NV,
  pattern GL_VIEWPORT_COMMAND_NV,
  -- * Functions
  glCallCommandListNV,
  glCommandListSegmentsNV,
  glCompileCommandListNV,
  glCreateCommandListsNV,
  glCreateStatesNV,
  glDeleteCommandListsNV,
  glDeleteStatesNV,
  glDrawCommandsAddressNV,
  glDrawCommandsNV,
  glDrawCommandsStatesAddressNV,
  glDrawCommandsStatesNV,
  glGetCommandHeaderNV,
  glGetStageIndexNV,
  glIsCommandListNV,
  glIsStateNV,
  glListDrawCommandsStatesClientNV,
  glStateCaptureNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
