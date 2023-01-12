{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TextureMultiBuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TextureMultiBuffer (
  -- * Extension Support
  glGetSGIXTextureMultiBuffer,
  gl_SGIX_texture_multi_buffer,
  -- * Enums
  pattern GL_TEXTURE_MULTI_BUFFER_HINT_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
