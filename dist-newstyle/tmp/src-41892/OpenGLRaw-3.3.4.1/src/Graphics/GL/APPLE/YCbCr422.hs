{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.YCbCr422
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.YCbCr422 (
  -- * Extension Support
  glGetAPPLEYCbCr422,
  gl_APPLE_ycbcr_422,
  -- * Enums
  pattern GL_UNSIGNED_SHORT_8_8_APPLE,
  pattern GL_UNSIGNED_SHORT_8_8_REV_APPLE,
  pattern GL_YCBCR_422_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
