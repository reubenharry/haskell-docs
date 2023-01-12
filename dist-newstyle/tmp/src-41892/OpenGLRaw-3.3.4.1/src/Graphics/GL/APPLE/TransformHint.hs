{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.APPLE.TransformHint
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.APPLE.TransformHint (
  -- * Extension Support
  glGetAPPLETransformHint,
  gl_APPLE_transform_hint,
  -- * Enums
  pattern GL_TRANSFORM_HINT_APPLE
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
