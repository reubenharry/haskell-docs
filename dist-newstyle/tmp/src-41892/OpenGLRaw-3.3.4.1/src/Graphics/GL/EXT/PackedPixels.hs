{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PackedPixels
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PackedPixels (
  -- * Extension Support
  glGetEXTPackedPixels,
  gl_EXT_packed_pixels,
  -- * Enums
  pattern GL_UNSIGNED_BYTE_3_3_2_EXT,
  pattern GL_UNSIGNED_INT_10_10_10_2_EXT,
  pattern GL_UNSIGNED_INT_8_8_8_8_EXT,
  pattern GL_UNSIGNED_SHORT_4_4_4_4_EXT,
  pattern GL_UNSIGNED_SHORT_5_5_5_1_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
