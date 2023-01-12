{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.CMYKA
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.CMYKA (
  -- * Extension Support
  glGetEXTCMYKA,
  gl_EXT_cmyka,
  -- * Enums
  pattern GL_CMYKA_EXT,
  pattern GL_CMYK_EXT,
  pattern GL_PACK_CMYK_HINT_EXT,
  pattern GL_UNPACK_CMYK_HINT_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
