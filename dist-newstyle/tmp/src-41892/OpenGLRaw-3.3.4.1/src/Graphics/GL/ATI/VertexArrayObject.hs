{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.VertexArrayObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.VertexArrayObject (
  -- * Extension Support
  glGetATIVertexArrayObject,
  gl_ATI_vertex_array_object,
  -- * Enums
  pattern GL_ARRAY_OBJECT_BUFFER_ATI,
  pattern GL_ARRAY_OBJECT_OFFSET_ATI,
  pattern GL_DISCARD_ATI,
  pattern GL_DYNAMIC_ATI,
  pattern GL_OBJECT_BUFFER_SIZE_ATI,
  pattern GL_OBJECT_BUFFER_USAGE_ATI,
  pattern GL_PRESERVE_ATI,
  pattern GL_STATIC_ATI,
  -- * Functions
  glArrayObjectATI,
  glFreeObjectBufferATI,
  glGetArrayObjectfvATI,
  glGetArrayObjectivATI,
  glGetObjectBufferfvATI,
  glGetObjectBufferivATI,
  glGetVariantArrayObjectfvATI,
  glGetVariantArrayObjectivATI,
  glIsObjectBufferATI,
  glNewObjectBufferATI,
  glUpdateObjectBufferATI,
  glVariantArrayObjectATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
