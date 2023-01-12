{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIS.GenerateMipmap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIS.GenerateMipmap (
  -- * Extension Support
  glGetSGISGenerateMipmap,
  gl_SGIS_generate_mipmap,
  -- * Enums
  pattern GL_GENERATE_MIPMAP_HINT_SGIS,
  pattern GL_GENERATE_MIPMAP_SGIS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
