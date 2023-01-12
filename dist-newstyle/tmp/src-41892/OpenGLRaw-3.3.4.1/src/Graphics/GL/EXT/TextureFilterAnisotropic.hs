{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureFilterAnisotropic
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureFilterAnisotropic (
  -- * Extension Support
  glGetEXTTextureFilterAnisotropic,
  gl_EXT_texture_filter_anisotropic,
  -- * Enums
  pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT,
  pattern GL_TEXTURE_MAX_ANISOTROPY_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
