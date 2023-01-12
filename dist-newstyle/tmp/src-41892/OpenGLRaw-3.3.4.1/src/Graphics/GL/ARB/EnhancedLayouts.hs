{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.EnhancedLayouts
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.EnhancedLayouts (
  -- * Extension Support
  glGetARBEnhancedLayouts,
  gl_ARB_enhanced_layouts,
  -- * Enums
  pattern GL_LOCATION_COMPONENT,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_INDEX,
  pattern GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
