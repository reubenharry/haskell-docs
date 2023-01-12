{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.TextureSharedExponent
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.TextureSharedExponent (
  -- * Extension Support
  glGetEXTTextureSharedExponent,
  gl_EXT_texture_shared_exponent,
  -- * Enums
  pattern GL_RGB9_E5_EXT,
  pattern GL_TEXTURE_SHARED_SIZE_EXT,
  pattern GL_UNSIGNED_INT_5_9_9_9_REV_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
