{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.ShadowAmbient
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.ShadowAmbient (
  -- * Extension Support
  glGetSGIXShadowAmbient,
  gl_SGIX_shadow_ambient,
  -- * Enums
  pattern GL_SHADOW_AMBIENT_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
