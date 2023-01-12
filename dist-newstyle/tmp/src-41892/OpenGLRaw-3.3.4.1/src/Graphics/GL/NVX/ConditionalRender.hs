--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NVX.ConditionalRender
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NVX.ConditionalRender (
  -- * Extension Support
  glGetNVXConditionalRender,
  gl_NVX_conditional_render,
  -- * Functions
  glBeginConditionalRenderNVX,
  glEndConditionalRenderNVX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
