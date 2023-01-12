{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.TextureLODBias
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.TextureLODBias (
  -- * Extension Support
  glGetSGIXTextureLODBias,
  gl_SGIX_texture_lod_bias,
  -- * Enums
  pattern GL_TEXTURE_LOD_BIAS_R_SGIX,
  pattern GL_TEXTURE_LOD_BIAS_S_SGIX,
  pattern GL_TEXTURE_LOD_BIAS_T_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
