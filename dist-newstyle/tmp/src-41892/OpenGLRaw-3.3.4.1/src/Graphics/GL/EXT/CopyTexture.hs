--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.CopyTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.CopyTexture (
  -- * Extension Support
  glGetEXTCopyTexture,
  gl_EXT_copy_texture,
  -- * Functions
  glCopyTexImage1DEXT,
  glCopyTexImage2DEXT,
  glCopyTexSubImage1DEXT,
  glCopyTexSubImage2DEXT,
  glCopyTexSubImage3DEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
