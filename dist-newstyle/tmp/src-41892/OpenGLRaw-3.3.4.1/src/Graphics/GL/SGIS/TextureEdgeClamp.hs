{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.TextureEdgeClamp
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.TextureEdgeClamp (
  -- * Extension Support
  glGetSGISTextureEdgeClamp,
  gl_SGIS_texture_edge_clamp,
  -- * Enums
  pattern GL_CLAMP_TO_EDGE_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
