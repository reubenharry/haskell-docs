{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.DepthTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.DepthTexture (
  -- * Extension Support
  glGetSGIXDepthTexture,
  gl_SGIX_depth_texture,
  -- * Enums
  pattern GL_DEPTH_COMPONENT16_SGIX,
  pattern GL_DEPTH_COMPONENT24_SGIX,
  pattern GL_DEPTH_COMPONENT32_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
