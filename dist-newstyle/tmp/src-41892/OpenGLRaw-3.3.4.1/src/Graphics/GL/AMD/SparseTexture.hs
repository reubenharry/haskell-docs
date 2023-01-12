{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.SparseTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.SparseTexture (
  -- * Extension Support
  glGetAMDSparseTexture,
  gl_AMD_sparse_texture,
  -- * Enums
  pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_AMD,
  pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS,
  pattern GL_MAX_SPARSE_TEXTURE_SIZE_AMD,
  pattern GL_MIN_LOD_WARNING_AMD,
  pattern GL_MIN_SPARSE_LEVEL_AMD,
  pattern GL_TEXTURE_STORAGE_SPARSE_BIT_AMD,
  pattern GL_VIRTUAL_PAGE_SIZE_X_AMD,
  pattern GL_VIRTUAL_PAGE_SIZE_Y_AMD,
  pattern GL_VIRTUAL_PAGE_SIZE_Z_AMD,
  -- * Functions
  glTexStorageSparseAMD,
  glTextureStorageSparseAMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
