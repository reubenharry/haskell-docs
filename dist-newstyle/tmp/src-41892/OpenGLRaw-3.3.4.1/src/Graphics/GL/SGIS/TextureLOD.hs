{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureLOD
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureLOD (
  -- * Extension Support
  glGetSGISTextureLOD,
  gl_SGIS_texture_lod,
  -- * Enums
  pattern GL_TEXTURE_BASE_LEVEL_SGIS,
  pattern GL_TEXTURE_MAX_LEVEL_SGIS,
  pattern GL_TEXTURE_MAX_LOD_SGIS,
  pattern GL_TEXTURE_MIN_LOD_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
