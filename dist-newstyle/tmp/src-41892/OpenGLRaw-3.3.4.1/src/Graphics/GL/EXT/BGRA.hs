{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BGRA
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BGRA (
  -- * Extension Support
  glGetEXTBGRA,
  gl_EXT_bgra,
  -- * Enums
  pattern GL_BGRA_EXT,
  pattern GL_BGR_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
