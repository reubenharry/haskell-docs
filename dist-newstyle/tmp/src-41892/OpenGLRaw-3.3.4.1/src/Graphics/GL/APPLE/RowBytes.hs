{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.RowBytes
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.RowBytes (
  -- * Extension Support
  glGetAPPLERowBytes,
  gl_APPLE_row_bytes,
  -- * Enums
  pattern GL_PACK_ROW_BYTES_APPLE,
  pattern GL_UNPACK_ROW_BYTES_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
