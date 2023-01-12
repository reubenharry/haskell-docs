{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OML.Interlace
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.OML.Interlace (
  -- * Extension Support
  glGetOMLInterlace,
  gl_OML_interlace,
  -- * Enums
  pattern GL_INTERLACE_OML,
  pattern GL_INTERLACE_READ_OML
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
