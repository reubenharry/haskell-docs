{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ARB.ES32Compatibility
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ARB.ES32Compatibility (
  -- * Extension Support
  glGetARBES32Compatibility,
  gl_ARB_ES3_2_compatibility,
  -- * Enums
  pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB,
  pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB,
  pattern GL_PRIMITIVE_BOUNDING_BOX_ARB,
  -- * Functions
  glPrimitiveBoundingBoxARB
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
import Graphics.GL.Functions
