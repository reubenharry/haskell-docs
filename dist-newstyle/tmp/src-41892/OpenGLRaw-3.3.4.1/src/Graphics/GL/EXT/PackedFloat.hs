{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT.PackedFloat
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT.PackedFloat (
  -- * Extension Support
  glGetEXTPackedFloat,
  gl_EXT_packed_float,
  -- * Enums
  pattern GL_R11F_G11F_B10F_EXT,
  pattern GL_RGBA_SIGNED_COMPONENTS_EXT,
  pattern GL_UNSIGNED_INT_10F_11F_11F_REV_EXT
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
