{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.ConditionalRender
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.ConditionalRender (
  -- * Extension Support
  glGetNVConditionalRender,
  gl_NV_conditional_render,
  -- * Enums
  pattern GL_QUERY_BY_REGION_NO_WAIT_NV,
  pattern GL_QUERY_BY_REGION_WAIT_NV,
  pattern GL_QUERY_NO_WAIT_NV,
  pattern GL_QUERY_WAIT_NV,
  -- * Functions
  glBeginConditionalRenderNV,
  glEndConditionalRenderNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
