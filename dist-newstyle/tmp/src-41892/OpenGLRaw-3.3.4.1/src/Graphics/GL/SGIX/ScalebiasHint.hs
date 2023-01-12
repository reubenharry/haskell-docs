{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.ScalebiasHint
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.ScalebiasHint (
  -- * Extension Support
  glGetSGIXScalebiasHint,
  gl_SGIX_scalebias_hint,
  -- * Enums
  pattern GL_SCALEBIAS_HINT_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
