--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.BindlessTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.BindlessTexture (
  -- * Extension Support
  glGetNVBindlessTexture,
  gl_NV_bindless_texture,
  -- * Functions
  glGetImageHandleNV,
  glGetTextureHandleNV,
  glGetTextureSamplerHandleNV,
  glIsImageHandleResidentNV,
  glIsTextureHandleResidentNV,
  glMakeImageHandleNonResidentNV,
  glMakeImageHandleResidentNV,
  glMakeTextureHandleNonResidentNV,
  glMakeTextureHandleResidentNV,
  glProgramUniformHandleui64NV,
  glProgramUniformHandleui64vNV,
  glUniformHandleui64NV,
  glUniformHandleui64vNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
