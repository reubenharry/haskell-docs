{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.AsyncHistogram
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.AsyncHistogram (
  -- * Extension Support
  glGetSGIXAsyncHistogram,
  gl_SGIX_async_histogram,
  -- * Enums
  pattern GL_ASYNC_HISTOGRAM_SGIX,
  pattern GL_MAX_ASYNC_HISTOGRAM_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
