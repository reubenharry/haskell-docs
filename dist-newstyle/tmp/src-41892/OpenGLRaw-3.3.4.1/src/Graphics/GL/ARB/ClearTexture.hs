{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ClearTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ClearTexture (
  -- * Extension Support
  glGetARBClearTexture,
  gl_ARB_clear_texture,
  -- * Enums
  pattern GL_CLEAR_TEXTURE,
  -- * Functions
  glClearTexImage,
  glClearTexSubImage
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
