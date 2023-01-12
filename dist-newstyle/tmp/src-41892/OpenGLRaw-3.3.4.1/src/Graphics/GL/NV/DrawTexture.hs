--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.DrawTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.DrawTexture (
  -- * Extension Support
  glGetNVDrawTexture,
  gl_NV_draw_texture,
  -- * Functions
  glDrawTextureNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
