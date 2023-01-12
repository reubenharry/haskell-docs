{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureBorderClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureBorderClamp (
  -- * Extension Support
  glGetARBTextureBorderClamp,
  gl_ARB_texture_border_clamp,
  -- * Enums
  pattern GL_CLAMP_TO_BORDER_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
