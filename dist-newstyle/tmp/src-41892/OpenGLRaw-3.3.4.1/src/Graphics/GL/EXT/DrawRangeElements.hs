{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.DrawRangeElements
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.DrawRangeElements (
  -- * Extension Support
  glGetEXTDrawRangeElements,
  gl_EXT_draw_range_elements,
  -- * Enums
  pattern GL_MAX_ELEMENTS_INDICES_EXT,
  pattern GL_MAX_ELEMENTS_VERTICES_EXT,
  -- * Functions
  glDrawRangeElementsEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
