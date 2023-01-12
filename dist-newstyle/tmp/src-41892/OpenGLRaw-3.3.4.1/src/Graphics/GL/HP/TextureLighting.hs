{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.HP.TextureLighting
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.HP.TextureLighting (
  -- * Extension Support
  glGetHPTextureLighting,
  gl_HP_texture_lighting,
  -- * Enums
  pattern GL_TEXTURE_LIGHTING_MODE_HP,
  pattern GL_TEXTURE_POST_SPECULAR_HP,
  pattern GL_TEXTURE_PRE_SPECULAR_HP
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
