{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.DetailTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.DetailTexture (
  -- * Extension Support
  glGetSGISDetailTexture,
  gl_SGIS_detail_texture,
  -- * Enums
  pattern GL_DETAIL_TEXTURE_2D_BINDING_SGIS,
  pattern GL_DETAIL_TEXTURE_2D_SGIS,
  pattern GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS,
  pattern GL_DETAIL_TEXTURE_LEVEL_SGIS,
  pattern GL_DETAIL_TEXTURE_MODE_SGIS,
  pattern GL_LINEAR_DETAIL_ALPHA_SGIS,
  pattern GL_LINEAR_DETAIL_COLOR_SGIS,
  pattern GL_LINEAR_DETAIL_SGIS,
  -- * Functions
  glDetailTexFuncSGIS,
  glGetDetailTexFuncSGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
