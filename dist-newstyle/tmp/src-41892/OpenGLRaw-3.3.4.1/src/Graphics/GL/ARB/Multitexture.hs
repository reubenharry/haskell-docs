{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.Multitexture
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.Multitexture (
  -- * Extension Support
  glGetARBMultitexture,
  gl_ARB_multitexture,
  -- * Enums
  pattern GL_ACTIVE_TEXTURE_ARB,
  pattern GL_CLIENT_ACTIVE_TEXTURE_ARB,
  pattern GL_MAX_TEXTURE_UNITS_ARB,
  pattern GL_TEXTURE0_ARB,
  pattern GL_TEXTURE10_ARB,
  pattern GL_TEXTURE11_ARB,
  pattern GL_TEXTURE12_ARB,
  pattern GL_TEXTURE13_ARB,
  pattern GL_TEXTURE14_ARB,
  pattern GL_TEXTURE15_ARB,
  pattern GL_TEXTURE16_ARB,
  pattern GL_TEXTURE17_ARB,
  pattern GL_TEXTURE18_ARB,
  pattern GL_TEXTURE19_ARB,
  pattern GL_TEXTURE1_ARB,
  pattern GL_TEXTURE20_ARB,
  pattern GL_TEXTURE21_ARB,
  pattern GL_TEXTURE22_ARB,
  pattern GL_TEXTURE23_ARB,
  pattern GL_TEXTURE24_ARB,
  pattern GL_TEXTURE25_ARB,
  pattern GL_TEXTURE26_ARB,
  pattern GL_TEXTURE27_ARB,
  pattern GL_TEXTURE28_ARB,
  pattern GL_TEXTURE29_ARB,
  pattern GL_TEXTURE2_ARB,
  pattern GL_TEXTURE30_ARB,
  pattern GL_TEXTURE31_ARB,
  pattern GL_TEXTURE3_ARB,
  pattern GL_TEXTURE4_ARB,
  pattern GL_TEXTURE5_ARB,
  pattern GL_TEXTURE6_ARB,
  pattern GL_TEXTURE7_ARB,
  pattern GL_TEXTURE8_ARB,
  pattern GL_TEXTURE9_ARB,
  -- * Functions
  glActiveTextureARB,
  glClientActiveTextureARB,
  glMultiTexCoord1dARB,
  glMultiTexCoord1dvARB,
  glMultiTexCoord1fARB,
  glMultiTexCoord1fvARB,
  glMultiTexCoord1iARB,
  glMultiTexCoord1ivARB,
  glMultiTexCoord1sARB,
  glMultiTexCoord1svARB,
  glMultiTexCoord2dARB,
  glMultiTexCoord2dvARB,
  glMultiTexCoord2fARB,
  glMultiTexCoord2fvARB,
  glMultiTexCoord2iARB,
  glMultiTexCoord2ivARB,
  glMultiTexCoord2sARB,
  glMultiTexCoord2svARB,
  glMultiTexCoord3dARB,
  glMultiTexCoord3dvARB,
  glMultiTexCoord3fARB,
  glMultiTexCoord3fvARB,
  glMultiTexCoord3iARB,
  glMultiTexCoord3ivARB,
  glMultiTexCoord3sARB,
  glMultiTexCoord3svARB,
  glMultiTexCoord4dARB,
  glMultiTexCoord4dvARB,
  glMultiTexCoord4fARB,
  glMultiTexCoord4fvARB,
  glMultiTexCoord4iARB,
  glMultiTexCoord4ivARB,
  glMultiTexCoord4sARB,
  glMultiTexCoord4svARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
