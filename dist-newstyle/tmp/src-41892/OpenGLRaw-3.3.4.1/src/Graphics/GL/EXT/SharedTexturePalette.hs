{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.SharedTexturePalette
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.SharedTexturePalette (
  -- * Extension Support
  glGetEXTSharedTexturePalette,
  gl_EXT_shared_texture_palette,
  -- * Enums
  pattern GL_SHARED_TEXTURE_PALETTE_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
