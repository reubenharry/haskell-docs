{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.InternalformatQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.InternalformatQuery (
  -- * Extension Support
  glGetARBInternalformatQuery,
  gl_ARB_internalformat_query,
  -- * Enums
  pattern GL_NUM_SAMPLE_COUNTS,
  -- * Functions
  glGetInternalformativ
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
