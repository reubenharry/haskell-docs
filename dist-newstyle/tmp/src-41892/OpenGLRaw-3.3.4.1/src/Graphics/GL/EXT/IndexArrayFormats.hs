{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.IndexArrayFormats
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.IndexArrayFormats (
  -- * Extension Support
  glGetEXTIndexArrayFormats,
  gl_EXT_index_array_formats,
  -- * Enums
  pattern GL_IUI_N3F_V2F_EXT,
  pattern GL_IUI_N3F_V3F_EXT,
  pattern GL_IUI_V2F_EXT,
  pattern GL_IUI_V3F_EXT,
  pattern GL_T2F_IUI_N3F_V2F_EXT,
  pattern GL_T2F_IUI_N3F_V3F_EXT,
  pattern GL_T2F_IUI_V2F_EXT,
  pattern GL_T2F_IUI_V3F_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
