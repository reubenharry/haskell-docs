{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Framezoom
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Framezoom (
  -- * Extension Support
  glGetSGIXFramezoom,
  gl_SGIX_framezoom,
  -- * Enums
  pattern GL_FRAMEZOOM_FACTOR_SGIX,
  pattern GL_FRAMEZOOM_SGIX,
  pattern GL_MAX_FRAMEZOOM_FACTOR_SGIX,
  -- * Functions
  glFrameZoomSGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
