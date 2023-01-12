{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.RescaleNormal
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.RescaleNormal (
  -- * Extension Support
  glGetEXTRescaleNormal,
  gl_EXT_rescale_normal,
  -- * Enums
  pattern GL_RESCALE_NORMAL_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
