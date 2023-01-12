{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ShaderThreadGroup
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ShaderThreadGroup (
  -- * Extension Support
  glGetNVShaderThreadGroup,
  gl_NV_shader_thread_group,
  -- * Enums
  pattern GL_SM_COUNT_NV,
  pattern GL_WARPS_PER_SM_NV,
  pattern GL_WARP_SIZE_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
