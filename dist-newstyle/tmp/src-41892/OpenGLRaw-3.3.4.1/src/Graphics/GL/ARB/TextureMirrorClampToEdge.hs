{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureMirrorClampToEdge
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureMirrorClampToEdge (
  -- * Extension Support
  glGetARBTextureMirrorClampToEdge,
  gl_ARB_texture_mirror_clamp_to_edge,
  -- * Enums
  pattern GL_MIRROR_CLAMP_TO_EDGE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
