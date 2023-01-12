{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.RGB422
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.RGB422 (
  -- * Extension Support
  glGetAPPLERGB422,
  gl_APPLE_rgb_422,
  -- * Enums
  pattern GL_RGB_422_APPLE,
  pattern GL_RGB_RAW_422_APPLE,
  pattern GL_UNSIGNED_SHORT_8_8_APPLE,
  pattern GL_UNSIGNED_SHORT_8_8_REV_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
