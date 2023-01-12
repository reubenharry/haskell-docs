{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TextureCoordinateClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TextureCoordinateClamp (
  -- * Extension Support
  glGetSGIXTextureCoordinateClamp,
  gl_SGIX_texture_coordinate_clamp,
  -- * Enums
  pattern GL_TEXTURE_MAX_CLAMP_R_SGIX,
  pattern GL_TEXTURE_MAX_CLAMP_S_SGIX,
  pattern GL_TEXTURE_MAX_CLAMP_T_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
