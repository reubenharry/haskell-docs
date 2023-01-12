{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.CopyDepthToColor
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.CopyDepthToColor (
  -- * Extension Support
  glGetNVCopyDepthToColor,
  gl_NV_copy_depth_to_color,
  -- * Enums
  pattern GL_DEPTH_STENCIL_TO_BGRA_NV,
  pattern GL_DEPTH_STENCIL_TO_RGBA_NV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
