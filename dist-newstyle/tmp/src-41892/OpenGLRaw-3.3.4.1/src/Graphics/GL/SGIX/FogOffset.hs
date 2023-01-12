{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.FogOffset
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.FogOffset (
  -- * Extension Support
  glGetSGIXFogOffset,
  gl_SGIX_fog_offset,
  -- * Enums
  pattern GL_FOG_OFFSET_SGIX,
  pattern GL_FOG_OFFSET_VALUE_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
