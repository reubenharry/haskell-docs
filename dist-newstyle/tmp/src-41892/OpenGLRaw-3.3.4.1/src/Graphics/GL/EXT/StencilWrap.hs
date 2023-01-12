{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.StencilWrap
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.StencilWrap (
  -- * Extension Support
  glGetEXTStencilWrap,
  gl_EXT_stencil_wrap,
  -- * Enums
  pattern GL_DECR_WRAP_EXT,
  pattern GL_INCR_WRAP_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
