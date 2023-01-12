{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.IndexFunc
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.IndexFunc (
  -- * Extension Support
  glGetEXTIndexFunc,
  gl_EXT_index_func,
  -- * Enums
  pattern GL_INDEX_TEST_EXT,
  pattern GL_INDEX_TEST_FUNC_EXT,
  pattern GL_INDEX_TEST_REF_EXT,
  -- * Functions
  glIndexFuncEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
