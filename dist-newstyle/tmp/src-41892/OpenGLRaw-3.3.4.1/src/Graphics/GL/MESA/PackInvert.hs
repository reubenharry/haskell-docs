{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.MESA.PackInvert
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.MESA.PackInvert (
  -- * Extension Support
  glGetMESAPackInvert,
  gl_MESA_pack_invert,
  -- * Enums
  pattern GL_PACK_INVERT_MESA
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
