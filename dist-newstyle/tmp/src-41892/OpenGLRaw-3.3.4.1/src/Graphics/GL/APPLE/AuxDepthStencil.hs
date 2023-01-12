{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.AuxDepthStencil
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.AuxDepthStencil (
  -- * Extension Support
  glGetAPPLEAuxDepthStencil,
  gl_APPLE_aux_depth_stencil,
  -- * Enums
  pattern GL_AUX_DEPTH_STENCIL_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
