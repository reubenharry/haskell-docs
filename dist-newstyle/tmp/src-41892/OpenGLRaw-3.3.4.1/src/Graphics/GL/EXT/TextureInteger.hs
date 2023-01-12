{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureInteger
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureInteger (
  -- * Extension Support
  glGetEXTTextureInteger,
  gl_EXT_texture_integer,
  -- * Enums
  pattern GL_ALPHA16I_EXT,
  pattern GL_ALPHA16UI_EXT,
  pattern GL_ALPHA32I_EXT,
  pattern GL_ALPHA32UI_EXT,
  pattern GL_ALPHA8I_EXT,
  pattern GL_ALPHA8UI_EXT,
  pattern GL_ALPHA_INTEGER_EXT,
  pattern GL_BGRA_INTEGER_EXT,
  pattern GL_BGR_INTEGER_EXT,
  pattern GL_BLUE_INTEGER_EXT,
  pattern GL_GREEN_INTEGER_EXT,
  pattern GL_INTENSITY16I_EXT,
  pattern GL_INTENSITY16UI_EXT,
  pattern GL_INTENSITY32I_EXT,
  pattern GL_INTENSITY32UI_EXT,
  pattern GL_INTENSITY8I_EXT,
  pattern GL_INTENSITY8UI_EXT,
  pattern GL_LUMINANCE16I_EXT,
  pattern GL_LUMINANCE16UI_EXT,
  pattern GL_LUMINANCE32I_EXT,
  pattern GL_LUMINANCE32UI_EXT,
  pattern GL_LUMINANCE8I_EXT,
  pattern GL_LUMINANCE8UI_EXT,
  pattern GL_LUMINANCE_ALPHA16I_EXT,
  pattern GL_LUMINANCE_ALPHA16UI_EXT,
  pattern GL_LUMINANCE_ALPHA32I_EXT,
  pattern GL_LUMINANCE_ALPHA32UI_EXT,
  pattern GL_LUMINANCE_ALPHA8I_EXT,
  pattern GL_LUMINANCE_ALPHA8UI_EXT,
  pattern GL_LUMINANCE_ALPHA_INTEGER_EXT,
  pattern GL_LUMINANCE_INTEGER_EXT,
  pattern GL_RED_INTEGER_EXT,
  pattern GL_RGB16I_EXT,
  pattern GL_RGB16UI_EXT,
  pattern GL_RGB32I_EXT,
  pattern GL_RGB32UI_EXT,
  pattern GL_RGB8I_EXT,
  pattern GL_RGB8UI_EXT,
  pattern GL_RGBA16I_EXT,
  pattern GL_RGBA16UI_EXT,
  pattern GL_RGBA32I_EXT,
  pattern GL_RGBA32UI_EXT,
  pattern GL_RGBA8I_EXT,
  pattern GL_RGBA8UI_EXT,
  pattern GL_RGBA_INTEGER_EXT,
  pattern GL_RGBA_INTEGER_MODE_EXT,
  pattern GL_RGB_INTEGER_EXT,
  -- * Functions
  glClearColorIiEXT,
  glClearColorIuiEXT,
  glGetTexParameterIivEXT,
  glGetTexParameterIuivEXT,
  glTexParameterIivEXT,
  glTexParameterIuivEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
