{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OES.ReadFormat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.OES.ReadFormat (
  -- * Extension Support
  glGetOESReadFormat,
  gl_OES_read_format,
  -- * Enums
  pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES,
  pattern GL_IMPLEMENTATION_COLOR_READ_TYPE_OES
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
