{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.StencilTwoSide
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.StencilTwoSide (
  -- * Extension Support
  glGetEXTStencilTwoSide,
  gl_EXT_stencil_two_side,
  -- * Enums
  pattern GL_ACTIVE_STENCIL_FACE_EXT,
  pattern GL_STENCIL_TEST_TWO_SIDE_EXT,
  -- * Functions
  glActiveStencilFaceEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
