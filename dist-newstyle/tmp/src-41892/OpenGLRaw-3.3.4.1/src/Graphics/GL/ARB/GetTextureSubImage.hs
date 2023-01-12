--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.GetTextureSubImage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.GetTextureSubImage (
  -- * Extension Support
  glGetARBGetTextureSubImage,
  gl_ARB_get_texture_sub_image,
  -- * Functions
  glGetCompressedTextureSubImage,
  glGetTextureSubImage
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
