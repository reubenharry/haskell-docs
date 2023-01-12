{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.EnvmapBumpmap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.EnvmapBumpmap (
  -- * Extension Support
  glGetATIEnvmapBumpmap,
  gl_ATI_envmap_bumpmap,
  -- * Enums
  pattern GL_BUMP_ENVMAP_ATI,
  pattern GL_BUMP_NUM_TEX_UNITS_ATI,
  pattern GL_BUMP_ROT_MATRIX_ATI,
  pattern GL_BUMP_ROT_MATRIX_SIZE_ATI,
  pattern GL_BUMP_TARGET_ATI,
  pattern GL_BUMP_TEX_UNITS_ATI,
  pattern GL_DU8DV8_ATI,
  pattern GL_DUDV_ATI,
  -- * Functions
  glGetTexBumpParameterfvATI,
  glGetTexBumpParameterivATI,
  glTexBumpParameterfvATI,
  glTexBumpParameterivATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
