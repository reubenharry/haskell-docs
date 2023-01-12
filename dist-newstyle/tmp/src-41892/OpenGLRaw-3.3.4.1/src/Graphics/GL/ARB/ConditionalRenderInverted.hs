{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ConditionalRenderInverted
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ConditionalRenderInverted (
  -- * Extension Support
  glGetARBConditionalRenderInverted,
  gl_ARB_conditional_render_inverted,
  -- * Enums
  pattern GL_QUERY_BY_REGION_NO_WAIT_INVERTED,
  pattern GL_QUERY_BY_REGION_WAIT_INVERTED,
  pattern GL_QUERY_NO_WAIT_INVERTED,
  pattern GL_QUERY_WAIT_INVERTED
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
