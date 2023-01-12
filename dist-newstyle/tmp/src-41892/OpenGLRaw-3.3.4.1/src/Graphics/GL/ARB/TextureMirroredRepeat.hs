{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureMirroredRepeat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureMirroredRepeat (
  -- * Extension Support
  glGetARBTextureMirroredRepeat,
  gl_ARB_texture_mirrored_repeat,
  -- * Enums
  pattern GL_MIRRORED_REPEAT_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
