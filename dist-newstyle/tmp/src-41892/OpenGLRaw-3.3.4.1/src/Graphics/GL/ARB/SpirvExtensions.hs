{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.SpirvExtensions
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.SpirvExtensions (
  -- * Extension Support
  glGetARBSpirvExtensions,
  gl_ARB_spirv_extensions,
  -- * Enums
  pattern GL_NUM_SPIR_V_EXTENSIONS,
  pattern GL_SPIR_V_EXTENSIONS
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
