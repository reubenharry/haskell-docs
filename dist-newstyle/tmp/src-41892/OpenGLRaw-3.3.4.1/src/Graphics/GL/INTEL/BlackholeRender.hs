{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.INTEL.BlackholeRender
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.INTEL.BlackholeRender (
  -- * Extension Support
  glGetINTELBlackholeRender,
  gl_INTEL_blackhole_render,
  -- * Enums
  pattern GL_BLACKHOLE_RENDER_INTEL
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
