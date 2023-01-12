{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Interlace
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Interlace (
  -- * Extension Support
  glGetSGIXInterlace,
  gl_SGIX_interlace,
  -- * Enums
  pattern GL_INTERLACE_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
