{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Subsample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Subsample (
  -- * Extension Support
  glGetSGIXSubsample,
  gl_SGIX_subsample,
  -- * Enums
  pattern GL_PACK_SUBSAMPLE_RATE_SGIX,
  pattern GL_PIXEL_SUBSAMPLE_2424_SGIX,
  pattern GL_PIXEL_SUBSAMPLE_4242_SGIX,
  pattern GL_PIXEL_SUBSAMPLE_4444_SGIX,
  pattern GL_UNPACK_SUBSAMPLE_RATE_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
