{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.YCrCb
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.YCrCb (
  -- * Extension Support
  glGetSGIXYCrCb,
  gl_SGIX_ycrcb,
  -- * Enums
  pattern GL_YCRCB_422_SGIX,
  pattern GL_YCRCB_444_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
