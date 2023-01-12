{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.MapBufferAlignment
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.MapBufferAlignment (
  -- * Extension Support
  glGetARBMapBufferAlignment,
  gl_ARB_map_buffer_alignment,
  -- * Enums
  pattern GL_MIN_MAP_BUFFER_ALIGNMENT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
