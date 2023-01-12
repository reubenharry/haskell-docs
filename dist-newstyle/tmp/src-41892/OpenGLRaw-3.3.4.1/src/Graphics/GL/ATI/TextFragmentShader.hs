{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.TextFragmentShader
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.TextFragmentShader (
  -- * Extension Support
  glGetATITextFragmentShader,
  gl_ATI_text_fragment_shader,
  -- * Enums
  pattern GL_TEXT_FRAGMENT_SHADER_ATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
