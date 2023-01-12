{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.PresentVideo
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.PresentVideo (
  -- * Extension Support
  glGetNVPresentVideo,
  gl_NV_present_video,
  -- * Enums
  pattern GL_CURRENT_TIME_NV,
  pattern GL_FIELDS_NV,
  pattern GL_FRAME_NV,
  pattern GL_NUM_FILL_STREAMS_NV,
  pattern GL_PRESENT_DURATION_NV,
  pattern GL_PRESENT_TIME_NV,
  -- * Functions
  glGetVideoi64vNV,
  glGetVideoivNV,
  glGetVideoui64vNV,
  glGetVideouivNV,
  glPresentFrameDualFillNV,
  glPresentFrameKeyedNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
