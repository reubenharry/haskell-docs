--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ThreeDFX.Tbuffer
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.ThreeDFX.Tbuffer (
  -- * Extension Support
  glGetThreeDFXTbuffer,
  gl_3DFX_tbuffer,
  -- * Functions
  glTbufferMask3DFX
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
