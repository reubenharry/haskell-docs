{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.OML.Resample
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.OML.Resample (
  -- * Extension Support
  glGetOMLResample,
  gl_OML_resample,
  -- * Enums
  pattern GL_PACK_RESAMPLE_OML,
  pattern GL_RESAMPLE_AVERAGE_OML,
  pattern GL_RESAMPLE_DECIMATE_OML,
  pattern GL_RESAMPLE_REPLICATE_OML,
  pattern GL_RESAMPLE_ZERO_FILL_OML,
  pattern GL_UNPACK_RESAMPLE_OML
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Tokens
