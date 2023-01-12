{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.SGIX.CalligraphicFragment
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.SGIX.CalligraphicFragment (
  -- * Extension Support
  glGetSGIXCalligraphicFragment,
  gl_SGIX_calligraphic_fragment,
  -- * Enums
  pattern GL_CALLIGRAPHIC_FRAGMENT_SGIX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
