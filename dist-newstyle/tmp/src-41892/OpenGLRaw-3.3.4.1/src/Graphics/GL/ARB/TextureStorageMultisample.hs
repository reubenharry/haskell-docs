--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.TextureStorageMultisample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.TextureStorageMultisample (
  -- * Extension Support
  glGetARBTextureStorageMultisample,
  gl_ARB_texture_storage_multisample,
  -- * Functions
  glTexStorage2DMultisample,
  glTexStorage3DMultisample
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
