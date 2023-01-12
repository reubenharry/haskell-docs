{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.WIN.SpecularFog
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.WIN.SpecularFog (
  -- * Extension Support
  glGetWINSpecularFog,
  gl_WIN_specular_fog,
  -- * Enums
  pattern GL_FOG_SPECULAR_TEXTURE_WIN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
