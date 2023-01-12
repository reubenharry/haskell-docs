{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ClipSpaceWScaling
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ClipSpaceWScaling (
  -- * Extension Support
  glGetNVClipSpaceWScaling,
  gl_NV_clip_space_w_scaling,
  -- * Enums
  pattern GL_VIEWPORT_POSITION_W_SCALE_NV,
  pattern GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV,
  pattern GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV,
  -- * Functions
  glViewportPositionWScaleNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
