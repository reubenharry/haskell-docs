{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.YCrCbA
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.YCrCbA (
  -- * Extension Support
  glGetSGIXYCrCbA,
  gl_SGIX_ycrcba,
  -- * Enums
  pattern GL_YCRCBA_SGIX,
  pattern GL_YCRCB_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
