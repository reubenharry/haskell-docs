{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.InternalformatSampleQuery
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.InternalformatSampleQuery (
  -- * Extension Support
  glGetNVInternalformatSampleQuery,
  gl_NV_internalformat_sample_query,
  -- * Enums
  pattern GL_CONFORMANT_NV,
  pattern GL_MULTISAMPLES_NV,
  pattern GL_RENDERBUFFER,
  pattern GL_SUPERSAMPLE_SCALE_X_NV,
  pattern GL_SUPERSAMPLE_SCALE_Y_NV,
  pattern GL_TEXTURE_2D_MULTISAMPLE,
  pattern GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
  -- * Functions
  glGetInternalformatSampleivNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
