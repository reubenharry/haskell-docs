{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BindableUniform
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BindableUniform (
  -- * Extension Support
  glGetEXTBindableUniform,
  gl_EXT_bindable_uniform,
  -- * Enums
  pattern GL_MAX_BINDABLE_UNIFORM_SIZE_EXT,
  pattern GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT,
  pattern GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT,
  pattern GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT,
  pattern GL_UNIFORM_BUFFER_BINDING_EXT,
  pattern GL_UNIFORM_BUFFER_EXT,
  -- * Functions
  glGetUniformBufferSizeEXT,
  glGetUniformOffsetEXT,
  glUniformBufferEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
