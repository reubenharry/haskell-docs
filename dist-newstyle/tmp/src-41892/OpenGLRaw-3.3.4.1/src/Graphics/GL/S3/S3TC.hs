{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.S3.S3TC
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.S3.S3TC (
  -- * Extension Support
  glGetS3S3TC,
  gl_S3_s3tc,
  -- * Enums
  pattern GL_RGB4_S3TC,
  pattern GL_RGBA4_DXT5_S3TC,
  pattern GL_RGBA4_S3TC,
  pattern GL_RGBA_DXT5_S3TC,
  pattern GL_RGBA_S3TC,
  pattern GL_RGB_S3TC
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
