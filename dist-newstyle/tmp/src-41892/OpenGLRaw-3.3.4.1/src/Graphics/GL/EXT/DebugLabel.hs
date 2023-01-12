{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DebugLabel
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DebugLabel (
  -- * Extension Support
  glGetEXTDebugLabel,
  gl_EXT_debug_label,
  -- * Enums
  pattern GL_BUFFER_OBJECT_EXT,
  pattern GL_PROGRAM_OBJECT_EXT,
  pattern GL_PROGRAM_PIPELINE_OBJECT_EXT,
  pattern GL_QUERY_OBJECT_EXT,
  pattern GL_SAMPLER,
  pattern GL_SHADER_OBJECT_EXT,
  pattern GL_TRANSFORM_FEEDBACK,
  pattern GL_VERTEX_ARRAY_OBJECT_EXT,
  -- * Functions
  glGetObjectLabelEXT,
  glLabelObjectEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
