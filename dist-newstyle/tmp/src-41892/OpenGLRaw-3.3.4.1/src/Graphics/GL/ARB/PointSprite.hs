{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.PointSprite
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.PointSprite (
  -- * Extension Support
  glGetARBPointSprite,
  gl_ARB_point_sprite,
  -- * Enums
  pattern GL_COORD_REPLACE_ARB,
  pattern GL_POINT_SPRITE_ARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
