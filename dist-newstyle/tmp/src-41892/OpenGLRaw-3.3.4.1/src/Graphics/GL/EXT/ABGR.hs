{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.ABGR
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.ABGR (
  -- * Extension Support
  glGetEXTABGR,
  gl_EXT_abgr,
  -- * Enums
  pattern GL_ABGR_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
