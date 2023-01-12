{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureObject
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureObject (
  -- * Extension Support
  glGetEXTTextureObject,
  gl_EXT_texture_object,
  -- * Enums
  pattern GL_TEXTURE_1D_BINDING_EXT,
  pattern GL_TEXTURE_2D_BINDING_EXT,
  pattern GL_TEXTURE_3D_BINDING_EXT,
  pattern GL_TEXTURE_PRIORITY_EXT,
  pattern GL_TEXTURE_RESIDENT_EXT,
  -- * Functions
  glAreTexturesResidentEXT,
  glBindTextureEXT,
  glDeleteTexturesEXT,
  glGenTexturesEXT,
  glIsTextureEXT,
  glPrioritizeTexturesEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
