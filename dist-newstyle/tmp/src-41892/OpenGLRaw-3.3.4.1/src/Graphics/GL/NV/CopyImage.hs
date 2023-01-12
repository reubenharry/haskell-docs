--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV.CopyImage
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.GL.NV.CopyImage (
  -- * Extension Support
  glGetNVCopyImage,
  gl_NV_copy_image,
  -- * Functions
  glCopyImageSubDataNV
) where

import Graphics.GL.ExtensionPredicates
import Graphics.GL.Functions
