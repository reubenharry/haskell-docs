{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.AMD.DepthClampSeparate
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.AMD.DepthClampSeparate (
  -- * Extension Support
  glGetAMDDepthClampSeparate,
  gl_AMD_depth_clamp_separate,
  -- * Enums
  pattern GL_DEPTH_CLAMP_FAR_AMD,
  pattern GL_DEPTH_CLAMP_NEAR_AMD
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
