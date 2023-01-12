{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureBorderClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureBorderClamp (
  -- * Extension Support
  glGetSGISTextureBorderClamp,
  gl_SGIS_texture_border_clamp,
  -- * Enums
  pattern GL_CLAMP_TO_BORDER_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
