{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.BlendFuncSeparate
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.BlendFuncSeparate (
  -- * Extension Support
  glGetEXTBlendFuncSeparate,
  gl_EXT_blend_func_separate,
  -- * Enums
  pattern GL_BLEND_DST_ALPHA_EXT,
  pattern GL_BLEND_DST_RGB_EXT,
  pattern GL_BLEND_SRC_ALPHA_EXT,
  pattern GL_BLEND_SRC_RGB_EXT,
  -- * Functions
  glBlendFuncSeparateEXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
