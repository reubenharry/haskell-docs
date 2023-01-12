{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ES3Compatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ES3Compatibility (
  -- * Extension Support
  glGetARBES3Compatibility,
  gl_ARB_ES3_compatibility,
  -- * Enums
  pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE,
  pattern GL_COMPRESSED_R11_EAC,
  pattern GL_COMPRESSED_RG11_EAC,
  pattern GL_COMPRESSED_RGB8_ETC2,
  pattern GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2,
  pattern GL_COMPRESSED_RGBA8_ETC2_EAC,
  pattern GL_COMPRESSED_SIGNED_R11_EAC,
  pattern GL_COMPRESSED_SIGNED_RG11_EAC,
  pattern GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC,
  pattern GL_COMPRESSED_SRGB8_ETC2,
  pattern GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2,
  pattern GL_MAX_ELEMENT_INDEX,
  pattern GL_PRIMITIVE_RESTART_FIXED_INDEX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
