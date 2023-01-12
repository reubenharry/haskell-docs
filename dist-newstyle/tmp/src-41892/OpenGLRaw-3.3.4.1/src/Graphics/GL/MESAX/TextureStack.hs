{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESAX.TextureStack
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESAX.TextureStack (
  -- * Extension Support
  glGetMESAXTextureStack,
  gl_MESAX_texture_stack,
  -- * Enums
  pattern GL_PROXY_TEXTURE_1D_STACK_MESAX,
  pattern GL_PROXY_TEXTURE_2D_STACK_MESAX,
  pattern GL_TEXTURE_1D_STACK_BINDING_MESAX,
  pattern GL_TEXTURE_1D_STACK_MESAX,
  pattern GL_TEXTURE_2D_STACK_BINDING_MESAX,
  pattern GL_TEXTURE_2D_STACK_MESAX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
