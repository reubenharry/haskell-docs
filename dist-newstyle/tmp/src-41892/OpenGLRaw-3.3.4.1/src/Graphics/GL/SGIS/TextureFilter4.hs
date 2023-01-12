{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureFilter4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureFilter4 (
  -- * Extension Support
  glGetSGISTextureFilter4,
  gl_SGIS_texture_filter4,
  -- * Enums
  pattern GL_FILTER4_SGIS,
  pattern GL_TEXTURE_FILTER4_SIZE_SGIS,
  -- * Functions
  glGetTexFilterFuncSGIS,
  glTexFilterFuncSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
