{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ShaderBufferStore
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ShaderBufferStore (
  -- * Extension Support
  glGetNVShaderBufferStore,
  gl_NV_shader_buffer_store,
  -- * Enums
  pattern GL_READ_WRITE,
  pattern GL_SHADER_GLOBAL_ACCESS_BARRIER_BIT_NV,
  pattern GL_WRITE_ONLY
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
