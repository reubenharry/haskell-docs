{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OML.Subsample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.OML.Subsample (
  -- * Extension Support
  glGetOMLSubsample,
  gl_OML_subsample,
  -- * Enums
  pattern GL_FORMAT_SUBSAMPLE_244_244_OML,
  pattern GL_FORMAT_SUBSAMPLE_24_24_OML
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
