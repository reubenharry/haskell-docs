{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Clipmap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Clipmap (
  -- * Extension Support
  glGetSGIXClipmap,
  gl_SGIX_clipmap,
  -- * Enums
  pattern GL_LINEAR_CLIPMAP_LINEAR_SGIX,
  pattern GL_LINEAR_CLIPMAP_NEAREST_SGIX,
  pattern GL_MAX_CLIPMAP_DEPTH_SGIX,
  pattern GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX,
  pattern GL_NEAREST_CLIPMAP_LINEAR_SGIX,
  pattern GL_NEAREST_CLIPMAP_NEAREST_SGIX,
  pattern GL_TEXTURE_CLIPMAP_CENTER_SGIX,
  pattern GL_TEXTURE_CLIPMAP_DEPTH_SGIX,
  pattern GL_TEXTURE_CLIPMAP_FRAME_SGIX,
  pattern GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX,
  pattern GL_TEXTURE_CLIPMAP_OFFSET_SGIX,
  pattern GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
