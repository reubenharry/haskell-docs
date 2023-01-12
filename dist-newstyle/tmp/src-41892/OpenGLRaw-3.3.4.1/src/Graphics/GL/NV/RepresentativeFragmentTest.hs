{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.RepresentativeFragmentTest
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.RepresentativeFragmentTest (
  -- * Extension Support
  glGetNVRepresentativeFragmentTest,
  gl_NV_representative_fragment_test,
  -- * Enums
  pattern GL_REPRESENTATIVE_FRAGMENT_TEST_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
