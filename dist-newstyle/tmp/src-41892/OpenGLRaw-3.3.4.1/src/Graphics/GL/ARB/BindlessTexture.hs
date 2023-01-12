{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.BindlessTexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.BindlessTexture (
  -- * Extension Support
  glGetARBBindlessTexture,
  gl_ARB_bindless_texture,
  -- * Enums
  pattern GL_UNSIGNED_INT64_ARB,
  -- * Functions
  glGetImageHandleARB,
  glGetTextureHandleARB,
  glGetTextureSamplerHandleARB,
  glGetVertexAttribLui64vARB,
  glIsImageHandleResidentARB,
  glIsTextureHandleResidentARB,
  glMakeImageHandleNonResidentARB,
  glMakeImageHandleResidentARB,
  glMakeTextureHandleNonResidentARB,
  glMakeTextureHandleResidentARB,
  glProgramUniformHandleui64ARB,
  glProgramUniformHandleui64vARB,
  glUniformHandleui64ARB,
  glUniformHandleui64vARB,
  glVertexAttribL1ui64ARB,
  glVertexAttribL1ui64vARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
