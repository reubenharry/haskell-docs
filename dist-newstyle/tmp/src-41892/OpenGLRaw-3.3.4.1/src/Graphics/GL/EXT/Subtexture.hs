--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.Subtexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.Subtexture (
  -- * Extension Support
  glGetEXTSubtexture,
  gl_EXT_subtexture,
  -- * Functions
  glTexSubImage1DEXT,
  glTexSubImage2DEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
