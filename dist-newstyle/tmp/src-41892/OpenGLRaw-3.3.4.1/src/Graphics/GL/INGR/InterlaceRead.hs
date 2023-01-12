{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INGR.InterlaceRead
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INGR.InterlaceRead (
  -- * Extension Support
  glGetINGRInterlaceRead,
  gl_INGR_interlace_read,
  -- * Enums
  pattern GL_INTERLACE_READ_INGR
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
