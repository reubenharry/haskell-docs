{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ATI.TextureEnvCombine3
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ATI.TextureEnvCombine3 (
  -- * Extension Support
  glGetATITextureEnvCombine3,
  gl_ATI_texture_env_combine3,
  -- * Enums
  pattern GL_MODULATE_ADD_ATI,
  pattern GL_MODULATE_SIGNED_ADD_ATI,
  pattern GL_MODULATE_SUBTRACT_ATI
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
