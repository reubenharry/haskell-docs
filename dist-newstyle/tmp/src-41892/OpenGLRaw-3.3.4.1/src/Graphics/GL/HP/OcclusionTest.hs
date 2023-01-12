{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.HP.OcclusionTest
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.HP.OcclusionTest (
  -- * Extension Support
  glGetHPOcclusionTest,
  gl_HP_occlusion_test,
  -- * Enums
  pattern GL_OCCLUSION_TEST_HP,
  pattern GL_OCCLUSION_TEST_RESULT_HP
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
