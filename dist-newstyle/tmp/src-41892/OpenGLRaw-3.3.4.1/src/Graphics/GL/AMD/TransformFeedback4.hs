{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.TransformFeedback4
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.TransformFeedback4 (
  -- * Extension Support
  glGetAMDTransformFeedback4,
  gl_AMD_transform_feedback4,
  -- * Enums
  pattern GL_STREAM_RASTERIZATION_AMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
