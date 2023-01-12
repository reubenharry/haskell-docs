{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.WIN.PhongShading
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.WIN.PhongShading (
  -- * Extension Support
  glGetWINPhongShading,
  gl_WIN_phong_shading,
  -- * Enums
  pattern GL_PHONG_HINT_WIN,
  pattern GL_PHONG_WIN
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
