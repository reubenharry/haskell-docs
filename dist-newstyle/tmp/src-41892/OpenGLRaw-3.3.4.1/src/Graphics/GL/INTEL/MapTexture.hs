{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.MapTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.MapTexture (
  -- * Extension Support
  glGetINTELMapTexture,
  gl_INTEL_map_texture,
  -- * Enums
  pattern GL_LAYOUT_DEFAULT_INTEL,
  pattern GL_LAYOUT_LINEAR_CPU_CACHED_INTEL,
  pattern GL_LAYOUT_LINEAR_INTEL,
  pattern GL_TEXTURE_MEMORY_LAYOUT_INTEL,
  -- * Functions
  glMapTexture2DINTEL,
  glSyncTextureINTEL,
  glUnmapTexture2DINTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
