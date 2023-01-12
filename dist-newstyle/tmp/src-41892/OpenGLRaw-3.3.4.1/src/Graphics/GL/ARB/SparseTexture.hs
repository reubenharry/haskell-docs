{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SparseTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SparseTexture (
  -- * Extension Support
  glGetARBSparseTexture,
  gl_ARB_sparse_texture,
  -- * Enums
  pattern GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB,
  pattern GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB,
  pattern GL_MAX_SPARSE_TEXTURE_SIZE_ARB,
  pattern GL_NUM_SPARSE_LEVELS_ARB,
  pattern GL_NUM_VIRTUAL_PAGE_SIZES_ARB,
  pattern GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB,
  pattern GL_TEXTURE_SPARSE_ARB,
  pattern GL_VIRTUAL_PAGE_SIZE_INDEX_ARB,
  pattern GL_VIRTUAL_PAGE_SIZE_X_ARB,
  pattern GL_VIRTUAL_PAGE_SIZE_Y_ARB,
  pattern GL_VIRTUAL_PAGE_SIZE_Z_ARB,
  -- * Functions
  glTexPageCommitmentARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
