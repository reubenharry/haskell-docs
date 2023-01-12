{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.TextureMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.TextureMultisample (
  -- * Extension Support
  glGetNVTextureMultisample,
  gl_NV_texture_multisample,
  -- * Enums
  pattern GL_TEXTURE_COLOR_SAMPLES_NV,
  pattern GL_TEXTURE_COVERAGE_SAMPLES_NV,
  -- * Functions
  glTexImage2DMultisampleCoverageNV,
  glTexImage3DMultisampleCoverageNV,
  glTextureImage2DMultisampleCoverageNV,
  glTextureImage2DMultisampleNV,
  glTextureImage3DMultisampleCoverageNV,
  glTextureImage3DMultisampleNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
