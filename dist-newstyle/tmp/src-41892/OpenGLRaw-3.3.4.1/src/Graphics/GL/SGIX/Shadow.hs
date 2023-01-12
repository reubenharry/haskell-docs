{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.Shadow
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.Shadow (
  -- * Extension Support
  glGetSGIXShadow,
  gl_SGIX_shadow,
  -- * Enums
  pattern GL_TEXTURE_COMPARE_OPERATOR_SGIX,
  pattern GL_TEXTURE_COMPARE_SGIX,
  pattern GL_TEXTURE_GEQUAL_R_SGIX,
  pattern GL_TEXTURE_LEQUAL_R_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
