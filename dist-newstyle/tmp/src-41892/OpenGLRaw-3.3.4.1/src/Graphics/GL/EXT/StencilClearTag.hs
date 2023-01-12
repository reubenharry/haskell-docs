{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.StencilClearTag
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.StencilClearTag (
  -- * Extension Support
  glGetEXTStencilClearTag,
  gl_EXT_stencil_clear_tag,
  -- * Enums
  pattern GL_STENCIL_CLEAR_TAG_VALUE_EXT,
  pattern GL_STENCIL_TAG_BITS_EXT,
  -- * Functions
  glStencilClearTagEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
