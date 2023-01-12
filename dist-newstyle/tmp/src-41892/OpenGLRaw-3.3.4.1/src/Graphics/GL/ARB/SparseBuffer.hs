{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SparseBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SparseBuffer (
  -- * Extension Support
  glGetARBSparseBuffer,
  gl_ARB_sparse_buffer,
  -- * Enums
  pattern GL_SPARSE_BUFFER_PAGE_SIZE_ARB,
  pattern GL_SPARSE_STORAGE_BIT_ARB,
  -- * Functions
  glBufferPageCommitmentARB,
  glNamedBufferPageCommitmentARB,
  glNamedBufferPageCommitmentEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
