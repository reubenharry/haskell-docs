--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.VDPAUInterop2
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.VDPAUInterop2 (
  -- * Extension Support
  glGetNVVDPAUInterop2,
  gl_NV_vdpau_interop2,
  -- * Functions
  glVDPAURegisterVideoSurfaceWithPictureStructureNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
