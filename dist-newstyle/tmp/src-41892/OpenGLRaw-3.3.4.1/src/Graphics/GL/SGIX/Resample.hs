{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Resample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Resample (
  -- * Extension Support
  glGetSGIXResample,
  gl_SGIX_resample,
  -- * Enums
  pattern GL_PACK_RESAMPLE_SGIX,
  pattern GL_RESAMPLE_DECIMATE_SGIX,
  pattern GL_RESAMPLE_REPLICATE_SGIX,
  pattern GL_RESAMPLE_ZERO_FILL_SGIX,
  pattern GL_UNPACK_RESAMPLE_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
