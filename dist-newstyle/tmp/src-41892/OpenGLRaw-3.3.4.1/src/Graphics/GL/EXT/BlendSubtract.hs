{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BlendSubtract
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BlendSubtract (
  -- * Extension Support
  glGetEXTBlendSubtract,
  gl_EXT_blend_subtract,
  -- * Enums
  pattern GL_FUNC_REVERSE_SUBTRACT_EXT,
  pattern GL_FUNC_SUBTRACT_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
